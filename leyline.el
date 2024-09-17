;;; leyline.el --- Apply complex changes with large language models  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 17 September 2024
;; Homepage: https://github.com/leotaku/leyline
;; Keywords: convenience tools llm
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (ellama "0.11"))

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

;;; Code:

(define-error 'leyline-error "Leyline error")

(define-error
 'leyline-error-apply-diff
 "Diff is not applicable" 'leyline-error)

(defun leyline--diff-locations (diff-text source-file-name)
  (let ((locations))
    (save-excursion
      (with-current-buffer (get-buffer-create "patch")
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

(defun leyline--apply-diff (diff-text &optional markers)
  (let ((locations (leyline--diff-locations diff-text (buffer-file-name))))
    (prog1 nil
      (pcase-dolist (`(,buf ,line-offset ,pos ,old ,new ,switched) locations)
        (unless (and (string= (car old) (buffer-substring (car pos) (cdr pos)))
                     (= (car pos) (save-excursion (goto-char (car pos)) (pos-bol))))
          (signal 'leyline-error-apply-diff nil)))
      (save-excursion
        (pcase-dolist (`(,buf ,line-offset ,pos ,old ,new ,switched) locations)
          (if markers
              (progn
                (goto-char (cdr pos))
                (insert (concat "=======\n"
                                (car new)
                                ">>>>>>> NEW\n"))
                (goto-char (car pos))
                (insert (concat "<<<<<<< HEAD\n")))
            (goto-char (car pos))
            (delete-region (car pos) (cdr pos))
            (insert (car new))))))))

(defun leyline--construct-prompt (task buffer-text &optional old-response)
  (concat (string-join '("You are a large language model and a careful programmer."
                         "Provide code and only code as output without any additional text, prompt or note.")
                       " ")
          "\n\n"
          "### Look at this code:\n\n"
          buffer-text
          "\n\n"
          (string-join '("From now on, only respond in the form of a minimal patch-compatible diff"
                         "INCLUDING @@ line numbers markers!"
                         "Make sure the line markers are absolutely correct!")
                       " ")
          "\n\n"
          "### This is your task:\n\n" task
          "\n\n"
          (if old-response
              (concat
               "### This is your previous response, but you failed to create a proper diff, please retry!"
               "\n\n" old-response "\n\n"))))

(defun leyline--create-debug-buffer (prompt)
  (with-current-buffer (get-buffer-create "*ellama-debug*")
    (delete-region (point-min) (point-max))
    (insert prompt)
    (current-buffer)))

(defun leyline--handle-response (response buffer debug-buffer)
  (with-current-buffer debug-buffer
    (insert response))
  (with-current-buffer buffer
    (condition-case err
        (leyline--apply-diff response)
      (leyline-error-apply-diff
       (when (y-or-n-p "Failed to recieve an apppropriate result, retry?")
         (leyline-buffer task response))))))

(defun leyline-buffer (task &optional old-response)
  (interactive "sTask: ")
  (let* ((full-prompt (leyline--construct-prompt task (buffer-string) old-response))
         (buffer (current-buffer))
         (debug-buffer (leyline--create-debug-buffer full-prompt)))
    (ellama-stream
     full-prompt
     :filter (lambda (chunk) "")
     :on-done (lambda (response) (leyline--handle-response response buffer debug-buffer)))))

(provide 'leyline)

;;; leyline.el ends here
