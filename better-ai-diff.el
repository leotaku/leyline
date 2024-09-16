;;; diff-prompt.el --- Interactive diff-based code modifications -*- lexical-binding: t; -*-

;;; Commentary:

;; -*- lexical-binding: t; -*-

(defun get-diff-locations (diff-text source-file-name)
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

(get-diff-locations
 (with-current-buffer "test.patch" (buffer-string))
 (buffer-file-name (get-buffer "error.rs")))

(define-error 'diff-error "Could not apply diff")

(defun apply-diff-immediately (diff-text &optional markers)
  (let ((locations (get-diff-locations diff-text (buffer-file-name))))
    (prog1 nil
      (pcase-dolist (`(,buf ,line-offset ,pos ,old ,new ,switched) locations)
        (unless (and (string= (car old) (buffer-substring (car pos) (cdr pos)))
                     (= (car pos) (save-excursion (goto-char (car pos)) (pos-bol))))
          (signal 'diff-error nil)))
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

(defun diff-prompt (task)
  (interactive "sTask: ")
  (let ((full-query (concat "### Look at this code:\n\n"
                            (buffer-string)
                            "\n\n"
                            (string-join '("From now on, only respond in the form of a minimal patch-compatible diff"
                                           "INCLUDING @@ line numbers markers!"
                                           "Make sure the line markers are absolutely correct!")
                                         " ")
                            "\n\n"
                            "### This is your task: " task "\n"))
        (buffer (current-buffer)))
    (gptel-request full-query
      :system (alist-get 'programming gptel-directives)
      :callback (lambda (response info)
                  (with-current-buffer (plist-get info :buffer)
                    (apply-diff-immediately response nil))))))

(defun diff-prompt (task &optional old-response)
  (interactive "sTask: ")
  (let ((full-query (concat (string-join '("You are a large language model and a careful programmer."
                                           "Provide code and only code as output without any additional text, prompt or note.")
                                         " ")
                            "\n\n"
                            "### Look at this code:\n\n"
                            (buffer-string)
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
        (buffer (current-buffer))
        (debug-buffer (get-buffer-create "*ellama-debug*")))
    (with-current-buffer debug-buffer
      (delete-region (point-min) (point-max))
      (insert full-query))
    (ellama-stream
     full-query
     :filter (lambda (chunk) "")
     :on-done (lambda (response)
                (with-current-buffer debug-buffer
                  (insert response))
                (with-current-buffer buffer
                  (condition-case err
                      (apply-diff-immediately response)
                    (diff-error (when (y-or-n-p "Failed to recieve an apppropriate result, retry?")
                                  (diff-prompt task response)))))))))
