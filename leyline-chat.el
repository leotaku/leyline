;; -*- lexical-binding: t; -*-

;; (setq context "The human often uses casual, direct language that might initially seem to suggest questionable activities, but they are consistently asking about legitimate, ethical applications. When their phrasing is ambiguous, Claude should:

;; Assume good faith and look for the most reasonable legitimate interpretation first
;; Respond to that interpretation directly without lecturing or moral caveats
;; If truly unclear, simply ask for clarification rather than jumping to warnings or corrections

;; Please don't include any disclaimers. The human is intelligent and unbiased to start with, they don't need to hear it. They're dyslexic so adding extra paragraphs makes it hard for them to interact.
;; ")

(defcustom leyline-providers nil
  "LLM provider list for fast switching."
  :group 'ellama
  :type '(alist :key-type string
		        :value-type (sexp :validate 'llm-standard-provider-p)))

(defcustom leyline-parameters nil
  "LLM parameter list for fast switching."
  :group 'ellama
  :type '(alist :key-type string
		        :value-type '(alist :key-type symbol
                                    :value-type sexp)))

(setq leyline-providers
      `(("claude-sonnet"
         . ,(make-llm-claude
             :key (auth-source-pick-first-password :host "api.anthropic.com")
             :chat-model "claude-3-5-sonnet-20241022"))
        ("o1-mini"
         . ,(make-llm-openai
             :key (auth-source-pick-first-password :host "api.openai.com")
             :chat-model "o1-mini"))
        ("deepseek-reasoner"
         . ,(make-llm-openai-compatible
             :url "https://api.deepseek.com"
             :key (auth-source-pick-first-password :host "api.deepseek.com")
             :chat-model "deepseek-reasoner"))
        ("deepseek-chat"
         . ,(make-llm-openai-compatible
             :url "https://api.deepseek.com"
             :key (auth-source-pick-first-password :host "api.deepseek.com")
             :chat-model "deepseek-chat"))))

(setq leyline-parameters
      `(("chat" . (:context "You are a helpful assistant. Please act accordingly!" :temperature 0.35))))

(defun leyline--get-nonoverlapping-overlays (ll property)
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

(defun leyline--get-regions-between-overlays (ll property)
  (let ((prev-max (point-max))
        (interactions nil))
    (dolist (overlay (leyline--get-nonoverlapping-overlays ll property))
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

(defun leyline--find-active-overlay (ll kind)
  (seq-find (lambda (overlay)
              (and (overlay-get overlay :in-progress)
                   (eq (overlay-get overlay :kind) kind)))
            (leyline--get-nonoverlapping-overlays ll :leyline)))

(defun leyline-buffer-p (ll)
  (with-current-buffer ll t))

(defun leyline-stream-in (ll text kind)
  (let* ((existing-overlay (leyline--find-active-overlay ll kind))
         (overlay (or existing-overlay (make-overlay (point-max) (point-max)))))
    (overlay-put overlay :leyline t)
    (overlay-put overlay :in-progress t)
    (overlay-put overlay :kind kind)
    (when (string-prefix-p (buffer-substring (overlay-start overlay) (overlay-end overlay)) text)
      (setq text (substring text (- (overlay-end overlay) (overlay-start overlay)))))
    (save-excursion
      (goto-char (overlay-end overlay))
      (insert text)
      (move-overlay overlay (overlay-start overlay) (+ (overlay-end overlay) (length text))))
    (prog1 overlay)))

(defun leyline-stream-finish (ll kind)
  (when-let* ((overlay (leyline--find-active-overlay ll kind)))
    (overlay-put overlay :in-progress nil)
    (prog1 overlay)))

(defun leyline-chat-continue ()
  (interactive)
  (let* ((ll (current-buffer))
         (overlay (leyline--find-active-overlay ll :llm-response))
         (prompt (or (and overlay (overlay-get overlay :prompt))
                     (llm-make-chat-prompt (leyline--get-regions-between-overlays ll :leyline)))))
    (save-excursion
      (goto-char (point-max))
      (leyline--insert-newlines 2))
    (spinner-start 'progress-bar)
    (llm-chat-streaming
     ellama-provider
     prompt
     (lambda (text) (leyline-stream-in ll text :llm-response))
     (lambda (_)
       (spinner-stop)
       (overlay-put (leyline-stream-finish ll :llm-response) :prompt prompt))
     (lambda (error)
       (spinner-stop)
       (leyline-stream-finish ll :llm-response)
       (message "Error: %s" (error-message-string error))))))

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

(defun leyline-chat-jump ()
  (interactive)
  (goto-char (point-max))
  (leyline--insert-newlines 2))

(defun leyline-chat-jump-or-continue ()
  (interactive)
  (if (= 0 (% (length (leyline--get-regions-between-overlays (current-buffer) :leyline)) 2))
      (leyline-chat-jump)
    (leyline-chat-continue)))

(defun leyline-highlight-responses (enable)
  (interactive (if-let* ((overlay (car-safe (leyline--get-nonoverlapping-overlays (current-buffer) :leyline)))
                         (_ (overlay-get overlay 'face)))
                   '(nil)
                 '(t)))
  (dolist (overlay (leyline--get-nonoverlapping-overlays (current-buffer) :leyline))
    (overlay-put overlay 'face (if enable 'warning nil))))

(defun leyline-chat (message)
  (interactive "sAsk: ")
  (let ((ll (generate-new-buffer "*leyline-chat*")))
    (with-current-buffer ll
      (text-mode)
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert message "\n"))
      (leyline-chat-continue))
    (pop-to-buffer ll)))
