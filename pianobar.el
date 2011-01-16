(require 'comint)

(defvar pianobar-buffer
  "*pianobar*"
  "The buffer name that pianobar will run in.")

(defvar pianobar-command
  "pianobar"
  "The command to run pianobar.")

(defvar pianobar-username nil
  "The Pandora username to use, or nil to prompt.")

(defvar pianobar-password nil
  "The Pandora password to use, or nil to prompt.")

(defvar pianobar-mode-hook nil
  "A list for storing pianobar-mode hooks.")

(defface pianobar-mode-prompt-face
  '((t :foreground "blue"))
  "Face to use to highlight prompts.")

(defface pianobar-mode-input-face
  '((t :weight bold))
  "Face to use to highlight user input.")

(defface pianobar-mode-info-face
  '((t :foreground "green"))
  "Face to use to highlight informative messages.")

(defface pianobar-mode-time-face
  '((t :inherit pianobar-mode-info-face :weight bold))
  "Face to use to highlight current song time. (due to bugs, this will only highlight non-current times.)")

(defvar pianobar-mode-font-lock-defaults
  '(("\\[\\?\\] \\(.*:\\) \\(.*\\)" (1 'pianobar-mode-prompt-face t) (2 'pianobar-mode-input-face t))
	("|> \\(.*\\)" 1 'pianobar-mode-info-face t)
	("# +\\(-[0-9]+:[0-9]+/[0-9]+:[0-9]+\\)\\(.*\\)" (1 'pianobar-mode-time-face t) (2 'pianobar-mode-input-face t)))
  "The default syntax-highlighting rules for pianobar-mode.")

(define-derived-mode pianobar-mode comint-mode "pianobar"
  "Major mode for interacting with pianobar.
\\{pianobar-mode-map}"
  
  (set (make-local-variable 'font-lock-defaults)
	   '(pianobar-mode-font-lock-defaults t))
  
  (set (make-local-variable 'comint-highlight-prompt)
	   'default)
  
  (set (make-local-variable 'comint-process-echoes) t))

(defun pianobar ()
  (interactive)
  (let ((buffer (get-buffer-create pianobar-buffer))
		(username pianobar-username)
		(password pianobar-password))
	
	(unless username
	  (setq username (read-from-minibuffer "Pandora username: ")))
	(unless password
	  (setq password (read-passwd "Pandora password: ")))
	
	(with-current-buffer buffer
	  (unless (comint-check-proc buffer)
		(make-comint-in-buffer "pianobar" buffer pianobar-command))
	  (comint-send-string buffer (concat username "\n"))
	  (comint-send-string buffer (concat password "\n"))
	  (pianobar-mode))
	
	(set-window-buffer (selected-window) buffer)))

(provide 'pianobar)
