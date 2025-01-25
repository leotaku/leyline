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

(define-minor-mode leyline-chat-mode
  "Minor for chat-like LLM interactions."
  :group 'leyline
  :keymap `(("\C-c\C-c" . leyline-chat-focus-or-continue)
            ("\C-c h" . leyline-highlight-responses)
            ("\C-c p" . leyline-select-provider)
            ("\C-c c" . leyline-select-configuration)))

(cl-defun leyline-overlays (&optional (ll (current-buffer)) (property :leyline))
  (let ((overlays nil))
    (with-current-buffer ll
      (save-excursion
        (goto-char 0)
        (while (/= (point) (point-max))
          (let ((overlay (seq-find (lambda (overlay) (overlay-get overlay property)) (overlays-at (point)))))
            (when (and overlay (not (eq overlay (car-safe overlays))))
              (push overlay overlays)))
          (goto-char (next-overlay-change (point))))))
    (prog1 overlays)))

(defun leyline-overlays-to-history (overlays)
  (let ((prev-max (point-max))
        (interactions nil))
    (dolist (overlay overlays)
      (push
       (string-trim (buffer-substring (overlay-end overlay) prev-max))
       interactions)
      (push
       (string-trim
        (buffer-substring (overlay-start overlay) (overlay-end overlay)))
       interactions)
      (setq prev-max (overlay-start overlay)))
    (push
     (string-trim (buffer-substring-no-properties (point-min) prev-max))
     interactions)
    (seq-filter (lambda (it) (length> it 0)) interactions)))

(cl-defun leyline-find-overlay (property-plist overlays &key property)
  (let* ((filter (lambda (overlay)
                   (seq-every-p
                    (lambda (kv) (eq (overlay-get overlay (car kv)) (cadr kv)))
                    (seq-partition property-plist 2))))
         (overlay (seq-find filter overlays)))
    (if (and overlay property)
        (overlay-get overlay property)
      overlay)))

(defun leyline-stream-in (ll text kind)
  (let* ((existing-overlay (leyline-find-overlay `(:in-progress t :kind ,kind) (leyline-overlays ll)))
         (overlay (or existing-overlay (make-overlay (point-max) (point-max)))))
    (overlay-put overlay :leyline t)
    (overlay-put overlay :in-progress t)
    (overlay-put overlay :kind kind)
    (when-let* ((face (leyline-find-overlay '() (leyline-overlays ll) :property 'face)))
      (overlay-put overlay 'face face))
    (when (string-prefix-p (buffer-substring (overlay-start overlay) (overlay-end overlay)) text)
      (setq text (substring text (- (overlay-end overlay) (overlay-start overlay)))))
    (save-excursion
      (goto-char (overlay-end overlay))
      (insert text)
      (move-overlay overlay (overlay-start overlay) (+ (overlay-end overlay) (length text))))
    (prog1 overlay)))

(defun leyline-stream-finish (ll kind)
  (when-let* ((overlay (leyline-find-overlay `(:in-progress t :kind ,kind) (leyline-overlays ll))))
    (overlay-put overlay :in-progress nil)
    (prog1 overlay)))

(defun leyline--insert-newlines (n)
  (save-excursion
    (goto-char (point-max))
    (when (= (pos-bol) (pos-eol))
      (setq n (1- n))
      (previous-line)
      (when (= (pos-bol) (pos-eol))
        (setq n (1- n)))))
  (dotimes (_ n)
    (insert "\n")))

(defun leyline-chat (message)
  (interactive "sAsk: ")
  (let ((ll (generate-new-buffer "*leyline-chat*")))
    (with-current-buffer ll
      (text-mode)
      (leyline-chat-mode +1)
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert message "\n"))
      (leyline-continue ll))
    (pop-to-buffer ll)))

(defun leyline-highlight-responses (&optional enable)
  (interactive (if (leyline-find-overlay '() (leyline-overlays) :property 'face) '(nil) '(t)))
  (dolist (overlay (leyline-overlays))
    (overlay-put overlay 'face (if enable 'warning nil))))

(defun leyline-chat-focus ()
  (interactive)
  (goto-char (point-max))
  (leyline--insert-newlines 2))

(defun leyline-chat-focus-or-continue ()
  (interactive)
  (if (= 0 (% (length (leyline-overlays-to-history (leyline-overlays))) 2))
      (leyline-chat-focus)
    (leyline-continue)))

(cl-defun leyline-continue (&optional (ll (current-buffer)))
  (interactive)
  (let* ((provider
          (leyline-select-provider nil))
         (prompt
          (apply #'llm-make-chat-prompt
                 (leyline-overlays-to-history (leyline-overlays ll))
                 (leyline-select-configuration nil))))
    (save-excursion
      (goto-char (point-max))
      (leyline--insert-newlines 2))
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

(provide 'leyline-chat)

;;; leyline-chat.el ends here
