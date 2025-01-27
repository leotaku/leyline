;;; leyline.el --- Higher-level large language model abstractions  -*- lexical-binding: t; -*-

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

(require 'llm)
(require 'llm-prompt)
(require 'spinner)

;;; Code:

(define-error 'leyline-error "Error")

(define-error
 'leyline-error-provider
 "LLM error" 'leyline-error)

(defgroup leyline nil
  "Tool for applying complex changes priovided by LLMs."
  :group 'tools)

(defcustom leyline-provider nil
  "Backend LLM provider."
  :group 'leyline
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom leyline-configuration nil
  "Backend LLM prompt and configuration."
  :group 'leyline
  :type '(alist :key-type symbol
		        :value-type sexp))

(defcustom leyline-providers nil
  "LLM provider list for fast switching."
  :group 'leyline
  :type '(alist :key-type string
		        :value-type (sexp :validate #'llm-standard-provider-p)))

(defcustom leyline-configurations nil
  "LLM parameter list for fast switching."
  :group 'leyline
  :type '(alist :key-type string
		        :value-type (sexp :validate #'listp)))

(defvar-local leyline-current nil
  "The running LLM request for the current buffer.")

;;; LLM configuration handling.

(defun leyline--assert-variable (variable alist prompt &optional force)
  (or (and (not force) (boundp variable) (symbol-value variable))
      (let ((key (completing-read prompt (mapcar #'car alist))))
        (make-local-variable variable)
        (set variable (alist-get key alist nil nil #'equal)))))

;;;###autoload
(defun leyline-select-provider (&optional force)
  "Select local `leyline-provider'."
  (interactive '(t))
  (leyline--assert-variable 'leyline-provider leyline-providers "Model: " force))

;;;###autoload
(defun leyline-select-configuration (&optional force)
  "Select local `leyline-configuration'."
  (interactive '(t))
  (leyline--assert-variable 'leyline-configuration leyline-configurations "Configuration: " force))

;;; Request handling.

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

;;; LLM "chat" abstractions.

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
    (save-excursion
      (goto-char (overlay-end overlay))
      (insert (substring text (or (overlay-get overlay :previous-length) 0)))
      (overlay-put overlay :previous-length (length text))
      (move-overlay overlay (overlay-start overlay) (+ (overlay-end overlay) (length text))))
    (prog1 overlay)))

(defun leyline-stream-finish (ll kind)
  (when-let* ((overlay (leyline-find-overlay `(:in-progress t :kind ,kind) (leyline-overlays ll))))
    (overlay-put overlay :in-progress nil)
    (prog1 overlay)))

;;; Utility functions

(defface leyline-highlight-face
  '((t :inherit italic))
  "Face for highlighting LLM responses."
  :group 'leyline)

(defun leyline-highlight-responses (&optional enable)
  (interactive (if (leyline-find-overlay '() (leyline-overlays) :property 'face) '(nil) '(t)))
  (dolist (overlay (leyline-overlays))
    (overlay-put overlay 'face (if enable 'leyline-highlight-face nil))))

(provide 'leyline)

;;; leyline.el ends here
