;;; leyline-chat.el --- Chat with large language models  -*- lexical-binding: t; -*-

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
;; TODO

(require 'leyline)

;;; Code:

(defun leyline--chat-insert-newlines (n)
  (save-excursion
    (goto-char (point-max))
    (when (= (pos-bol) (pos-eol))
      (setq n (1- n))
      (previous-line)
      (when (= (pos-bol) (pos-eol))
        (setq n (1- n)))))
  (dotimes (_ n)
    (insert "\n")))

(defun leyline-chat-focus ()
  (interactive)
  (goto-char (point-max))
  (leyline--chat-insert-newlines 2))

(cl-defun leyline-chat-continue (&optional (ll (current-buffer)))
  (interactive)
  (let* ((provider
          (leyline-select-provider nil))
         (prompt
          (apply #'llm-make-chat-prompt
                 (leyline-overlays-to-history (leyline-overlays ll))
                 (leyline-select-configuration nil))))
    (save-excursion
      (goto-char (point-max))
      (leyline--chat-insert-newlines 2))
    (leyline-request-mode +1)
    (setq-local
     leyline-current
     (llm-chat-streaming
      provider
      prompt
      (lambda (text) (leyline-stream-in ll text :llm-response))
      (lambda (_)
        (leyline-request-mode -1)
        (leyline-stream-finish ll :llm-response))
      (lambda (_ error)
        (leyline-request-mode -1)
        (leyline-stream-finish ll :llm-response)
        (message "Error: %s" (error-message-string error)))))))

(defun leyline-chat-focus-or-continue ()
  (interactive)
  (if (= 0 (% (length (leyline-overlays-to-history (leyline-overlays))) 2))
      (leyline-chat-focus)
    (leyline-chat-continue)))

(defun leyline-chat (message)
  (interactive "sAsk: ")
  (let ((ll (generate-new-buffer "*leyline-chat*")))
    (with-current-buffer ll
      (text-mode)
      (leyline-chat-mode +1)
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert message "\n"))
      (leyline-chat-continue ll))
    (pop-to-buffer ll)))

(define-minor-mode leyline-chat-mode
  "Minor for chat-like LLM interactions."
  :group 'leyline
  :keymap `(("\C-c\C-c" . leyline-chat-focus-or-continue)
            ("\C-c h" . leyline-highlight-responses)
            ("\C-c p" . leyline-select-provider)
            ("\C-c c" . leyline-select-configuration)))

(provide 'leyline-chat)

;;; leyline-chat.el ends here
