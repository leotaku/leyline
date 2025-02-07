;;; leyline-assistant.el --- Apply complex changes with large language models  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Leo Gaskin

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
;; complex changes to your code.  It provides a simple interface to interact with
;; AI models and apply the suggested changes directly to your buffer.  Leyline
;; uses diff-compatible patches to ensure precise modifications and offers
;; error handling and retry mechanisms for robust operation.

(require 'leyline)

;;; Code:

(define-error
 'leyline-error-diff
 "Change cannot be applied" 'leyline-error)

(llm-defprompt leyline-assistant-prompt
  "You are a large language model and a careful programmer.

Provide code and only code as output without any additional text, prompt or note.
Do not add an explanation! Do not add code block markers!
Do NOT add any code block markers you idiot!!!!

From now on, only respond in the form of a minimal patch-compatible diff!
Make sure the diffs, especially the old content are absolutely correct!
Return the most minimal possible semantic diffs!
Try to always include at least three non-empty lines of context.
Return earlier diff sections earlier in the response!!!

Look at this code:

{{code}}

This is your task:

{{task}}

")

(defun leyline--assistant-soft-signal (error-symbol data)
  (if debug-on-error
      (signal error-symbol data)
    (message "%s: %s" (or (get error-symbol 'error-message) "Error") data)))

(defun leyline--assistant-parse-chunks (diff-text)
  (with-current-buffer (get-buffer-create "*leyline-patch*")
    (delete-region (point-min) (point-max))
    (insert diff-text)
    (let ((result))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "---" line))
           ((string-prefix-p "+++" line))
           ((string-prefix-p "@" line)
            (push (list nil "" "") result))
           ((string-prefix-p "-" line)
            (setf (nth 1 (car result)) (concat (nth 1 (car result)) (substring line 1) "\n")))
           ((string-prefix-p "+" line)
            (setf (nth 2 (car result)) (concat (nth 2 (car result)) (substring line 1) "\n")))
           ((string-prefix-p " " line)
            (setf (nth 1 (car result)) (concat (nth 1 (car result)) (substring line 1) "\n"))
            (setf (nth 2 (car result)) (concat (nth 2 (car result)) (substring line 1) "\n")))))
        (forward-line))
      (prog1 result))))

(defun leyline--assistant-augument-chunks (chunks buffer-alist)
  (let ((result))
    (pcase-dolist (`(,buffer-name ,old ,new) chunks)
      (with-current-buffer (alist-get buffer-name buffer-alist)
        (save-excursion
          (goto-char 0)
          (if-let* ((end (search-forward old nil t nil))
                    (start (match-beginning 0)))
              (if (not (search-forward old nil t nil))
                  (push `(,(current-buffer) nil ,(cons start end) ,(cons old 0) ,(cons new 0) nil) result)
                (signal 'leyline-error-diff "Multiple matches"))
            (signal 'leyline-error-diff "No match")))))
    (nreverse result)))

(cl-defun leyline--assistant-needleman-wunsch-diff (a b &optional (score-fn #'equal) (indel-penalty -1))
  (let* ((m (make-vector (1+ (length a)) nil))
         (score-fn (lambda (a b) (if (funcall score-fn a b) 1 -1))))
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

(defun leyline--assistant-diff-skip-length (diff)
  (length (seq-take-while (lambda (it) (eq (car-safe it) 'keep)) diff)))

(defun leyline--assistant-apply-diff (diff-text)
  (let* ((parsed (leyline--assistant-parse-chunks diff-text))
         (locations (leyline--assistant-augument-chunks parsed `((nil . ,(current-buffer)))))
         (min (point-max))
         (max (point-min)))
    (prog1 nil
      (save-excursion
        (push (point) buffer-undo-list)
        (atomic-change-group
          (pcase-dolist (`(,_buf ,_ ,pos ,old ,new ,_) locations)
            (goto-char (car pos))
            (let ((diff (leyline--assistant-needleman-wunsch-diff (car old) (car new)))
                  (changed-end (+ (car pos) (length (car new)))))
              (dolist (action diff)
                (pcase-exhaustive action
                  (`(insert . ,char) (insert-char char))
                  (`(keep . ,_) (forward-char 1))
                  (`(delete . ,_) (delete-char 1))))
              (setq min (min min (+ (car pos) (leyline--assistant-diff-skip-length diff))))
              (setq max (max max (- changed-end (leyline--assistant-diff-skip-length (reverse diff)))))))))
      (when (<= min max)
        (pulse-momentary-highlight-region min max)))))

(defun leyline--assistant-create-debug-buffer (prompt)
  (with-current-buffer (get-buffer-create "*leyline-debug*")
    (delete-region (point-min) (point-max))
    (insert prompt)
    (insert "Response:\n\n")
    (current-buffer)))

(defun leyline--assistant-handle-response (task response buffer debug-buffer)
  (with-current-buffer debug-buffer
    (insert response))
  (with-current-buffer buffer
    (condition-case err
        (prog1 (leyline--assistant-apply-diff response)
          (setq-local leyline-current nil)
          (leyline-request-mode -1))
      (leyline-error-diff
       (leyline-request-mode -1)
       (when (y-or-n-p "Failed to receive an apppropriate result, retry?")
         (setq-local leyline-current (leyline--assistant-internal task response))
         (leyline-request-mode +1)))
      (error
       (leyline-request-mode -1)
       (leyline--assistant-soft-signal (car err) (cdr err))))))

(defun leyline--assistant-internal (task &optional old-response)
  (let* ((provider (leyline-select-provider nil))
         (full-prompt (llm-prompt-fill 'leyline-assistant-prompt provider
                                       :code (buffer-string) :task task))
         (buffer (current-buffer))
         (debug-buffer (leyline--assistant-create-debug-buffer full-prompt)))
    (llm-chat-async
     provider
     (llm-make-chat-prompt full-prompt)
     (lambda (response)
       (leyline--assistant-handle-response task response buffer debug-buffer))
     (lambda (error message)
       (setq-local leyline-current nil)
       (leyline-request-mode -1)
       (leyline--assistant-soft-signal 'leyline-error-provider message)))))

;;;###autoload
(defun leyline-assistant (task)
  "Apply changes specified in TASK to the current buffer."
  (interactive "*sTask: ")
  (leyline-request-mode +1)
  (setq-local leyline-current (leyline--assistant-internal task nil)))

(provide 'leyline-assistant)

;;; leyline-assistant.el ends here
