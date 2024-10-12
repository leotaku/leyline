;;; leyline.el --- Apply complex changes with large language models  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 17 September 2024
;; Homepage: https://github.com/leotaku/leyline
;; Keywords: convenience tools llm
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (llm "0.6.0") (spinner "1.7.4"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Leyline is an Emacs package that leverages large language models to apply
;; complex changes to your code. It provides a simple interface to interact with
;; AI models and apply the suggested changes directly to your buffer. Leyline
;; uses diff-compatible patches to ensure precise modifications and offers
;; error handling and retry mechanisms for robust operation.

(require 'llm)
(require 'spinner)

;;; Code:

(defgroup leyline nil
  "Tool for applying complex changes priovide by LLMs."
  :group 'tools)

(defcustom leyline-provider nil
  "Backend LLM provider."
  :group 'leyline
  :type '(sexp :validate 'llm-standard-provider-p))

(defvar-local leyline-current nil
  "The running LLM request for the current buffer.")

(define-error 'leyline-error "Leyline error")

(define-error
 'leyline-error-apply-diff
 "Diff is not applicable" 'leyline-error)

(define-error
 'leyline-error-provider
 "Provider error" 'leyline-error)

(defconst leyline-base-prompt
  (concat
   "You are a large language model and a careful programmer.\n"
   "Provide code and only code as output without any additional text, prompt or note.\n"
   "Do not add an explanation! Do not add code block markers!\n"
   "\n"
   "From now on, only respond in the form of a minimal patch-compatible diff INCLUDING @@ line numbers markers!\n"
   "Make sure the line markers are absolutely correct!\n"
   "Return the most minimal possible semantic diffs!\n"
   "Return earlier diff sections earlier in the response!!!"
   "\n"))

(defun leyline--construct-prompt (task buffer-text &optional old-response)
  (concat
   leyline-base-prompt
   "\n\n"
   "Look at this code:\n\n"
   buffer-text
   "\n\n"
   "This is your task:\n\n"
   task
   "\n\n"
   (when old-response
     (concat "This is your previous response, but you failed to create a proper diff, please retry!"
             "\n\n" old-response "\n\n"))))

(defun leyline--diff-locations (diff-text source-file-name)
  (let ((locations))
    (save-excursion
      (with-temp-buffer
        (setq-local diff-remembered-files-alist `((nil . ,source-file-name)))
        (delete-region (point-min) (point-max))
        (insert "--- " source-file-name "\n")
        (insert "+++ " source-file-name "\n")
        (insert diff-text)
        (diff-fixup-modifs (point-min) (point-max))
        (goto-char 0)
        (diff-hunk-next)
        (while (condition-case err
                   (save-excursion (prog1 t (diff-hunk-next)))
                 (error nil))
          (push (diff-find-source-location nil nil t) locations)
          (diff-hunk-next))))
    (prog1 locations)))

(cl-defun leyline--needleman-wunsch-diff (a b &optional (score-fn #'equal) (indel-penalty -1))
  (let* ((m (make-vector (1+ (length a)) nil))
         (score-fn (lambda (a b) (if (funcall score-fn a b) 1 -1)))
         (indel-penalty -1))
    (dotimes (I (1+ (length a)))
      (setf (aref m I) (make-vector (1+ (length b)) 0)))
    (dotimes (I (1+ (length a)))
      (setf (aref (aref m I) 0) (* I -1)))
    (dotimes (i (1+ (length b)))
      (setf (aref (aref m 0) i) (* i -1)))
    (dolist (I (number-sequence 1 (length a)))
      (dolist (i (number-sequence 1 (length b)))
        (let ((match (+ (aref (aref m (1- I)) (1- i))
                        (funcall score-fn (aref a (1- I)) (aref b (1- i)))))
              (delete (+ (aref (aref m (1- I)) i) indel-penalty))
              (insert (+ (aref (aref m I) (1- i)) indel-penalty)))
          (setf (aref (aref m I) i) (max match delete insert)))))
    (let ((I (length a))
          (i (length b))
          (result))
      (while (or (> i 0) (> I 0))
        (let ((diag (if (and (> I 0) (> i 0)) (aref (aref m (1- I)) (1- i)) -1000))
              (up (if (and (> I 0)) (aref (aref m (1- I)) i) -1000))
              (left (if (and (> i 0)) (aref (aref m I) (1- i)) -1000)))
          (if (and (>= diag up) (>= diag left))
              (progn
                (if (= (aref a (1- I)) (aref b (1- i)))
                    (push (cons 'keep (aref a (1- I))) result)
                  (push (cons 'insert (aref b (1- i))) result)
                  (push (cons 'delete (aref a (1- I))) result))
                (setq I (1- I) i (1- i)))
            (if (>= up left)
                (progn
                  (push (cons 'delete (aref a (1- I))) result)
                  (setq I (1- I)))
              (push (cons 'insert (aref b (1- i))) result)
              (setq i (1- i))))))
      (prog1 result))))

(defun leyline--diff-skip-length (diff)
  (length (seq-take-while (lambda (it) (eq (car-safe it) 'keep)) diff)))

(defun leyline--summarize-diff (diff)
  (let ((result))
    (pcase-dolist (`(,kind . ,value) diff)
      (if (and kind (eq kind (car-safe (car-safe result))))
          (setf (cdar result) (concat (cdar result) (char-to-string value)))
        (push (cons kind (char-to-string value)) result)))
    (nreverse result)))

(defun leyline--apply-diff (diff-text)
  (let ((locations (leyline--diff-locations diff-text (buffer-file-name)))
        (min (point-max))
        (max (point-min)))
    (prog1 nil
      (pcase-dolist (`(,buf ,line-offset ,pos ,old ,new ,switched) locations)
        (unless (and (string= (car old) (buffer-substring (car pos) (cdr pos)))
                     (= (car pos) (save-excursion (goto-char (car pos)) (line-beginning-position))))
          (signal 'leyline-error-apply-diff nil)))
      (save-excursion
        (push (point) buffer-undo-list)
        (atomic-change-group
          (pcase-dolist (`(,buf ,line-offset ,pos ,old ,new ,switched) locations)
            (goto-char (car pos))
            (let ((diff (leyline--needleman-wunsch-diff (car old) (car new)))
                  (changed-end (+ (car pos) (length (car new)))))
              (dolist (action diff)
                (pcase-exhaustive action
                  (`(insert . ,char) (insert-char char))
                  (`(keep . ,_) (forward-char 1))
                  (`(delete . ,_) (delete-char 1))))
              (setq min (min min (+ (car pos) (leyline--diff-skip-length diff))))
              (setq max (max max (- changed-end (leyline--diff-skip-length (reverse diff)))))))))
      (when (<= min max)
        (pulse-momentary-highlight-region min max)))))

(defun leyline--create-debug-buffer (prompt)
  (with-current-buffer (get-buffer-create "*leyline-debug*")
    (delete-region (point-min) (point-max))
    (insert prompt)
    (insert "Response:\n\n")
    (current-buffer)))

(defun leyline--handle-response (task response buffer debug-buffer)
  (with-current-buffer debug-buffer
    (insert response))
  (with-current-buffer buffer
    (condition-case err
        (prog1 (leyline--apply-diff response)
          (setq-local leyline-current nil)
          (leyline-request-mode -1))
      (leyline-error-apply-diff
       (if (y-or-n-p "Failed to receive an apppropriate result, retry?")
           (setq-local leyline-current (leyline--buffer-internal task response))
         (leyline-request-mode -1))))))

(defun leyline--buffer-internal (task &optional old-response)
  (let* ((full-prompt (leyline--construct-prompt task (buffer-string) old-response))
         (buffer (current-buffer))
         (debug-buffer (leyline--create-debug-buffer full-prompt)))
    (llm-chat-async
     leyline-provider
     (llm-make-chat-prompt full-prompt)
     (lambda (response)
       (leyline--handle-response task response buffer debug-buffer))
     (lambda (error message)
       (setq-local leyline-current nil)
       (leyline-request-mode -1)
       (signal 'leyline-error-provider message)))))

;;;###autoload
(defun leyline-buffer (task)
  "Apply changes specified in TASK to the current buffer."
  (interactive "*sTask: ")
  (leyline-request-mode +1)
  (setq-local leyline-current (leyline--buffer-internal task nil)))

;;;###autoload
(defun leyline-cancel (&optional request)
  "Cancel the given REQUEST or `leyline-current'."
  (interactive)
  (llm-cancel-request (or request leyline-current)))

;;;###autoload
(defun leyline-cancel-and-quit (&optional request)
  "Cancel the given REQUEST or `leyline-current' and quit."
  (interactive)
  (leyline-cancel request)
  (keyboard-quit))

(define-minor-mode leyline-request-mode
  "Minor mode for leyline buffers with an active request."
  :interactive nil
  :group 'leyline
  :keymap '(([remap keyboard-quit] . leyline-cancel-and-quit))
  (if leyline-request-mode
      (progn
        (add-hook 'kill-buffer-hook 'leyline-cancel nil t)
        (spinner-start 'progress-bar))
    (remove-hook 'kill-buffer-hook 'leyline-cancel)
    (spinner-stop)))

(provide 'leyline)

;;; leyline.el ends here
