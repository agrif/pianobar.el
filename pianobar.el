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

(defvar pianobar-prompt-regex
  "\\[\\?\\] .*: $"
  "A regex for matching a pianobar prompt.")

(defvar pianobar-mode-map
  (let ((map (nconc (make-keymap) comint-mode-map)))
	(substitute-key-definition 'self-insert-command 'pianobar-self-insert-command map global-map)
	map))

(defvar pianobar-mode-is-prompting nil
  "Whether pianobar is currently prompting, or not.")

(defun pianobar-mode-set-is-prompting (prompting)
  "Set whether pianobar is currently prompting for a string, or not."
  (with-current-buffer pianobar-buffer
	(set (make-local-variable 'pianobar-mode-is-prompting) prompting)
	(setq buffer-read-only (not prompting))))

(defun pianobar-output-filter (str)
  "Output filter for pianobar-mode."
  (pianobar-mode-set-is-prompting (string-match pianobar-prompt-regex str)))

(defun pianobar-self-insert-command (N)
  "Custom key-press handler for pianobar mode."
  (interactive "p")
  (if pianobar-mode-is-prompting
	  (self-insert-command N)
	(comint-send-string pianobar-buffer (char-to-string last-input-char))))

(define-derived-mode pianobar-mode comint-mode "pianobar"
  "Major mode for interacting with pianobar.
\\{pianobar-mode-map}"
  
  (set (make-local-variable 'font-lock-defaults)
	   '(pianobar-mode-font-lock-defaults t))
  
  (set (make-local-variable 'comint-process-echoes) t)
  (pianobar-mode-set-is-prompting nil)
  
  (add-hook 'comint-output-filter-functions 'pianobar-output-filter nil t))

(defun pianobar ()
  (interactive)
  (let ((username pianobar-username)
		(password pianobar-password))
	
	(unless username
	  (setq username (read-from-minibuffer "Pandora username: ")))
	(unless password
	  (setq password (read-passwd "Pandora password: ")))
	
	(if (and (stringp username) (stringp password))
		(let ((buffer (get-buffer-create pianobar-buffer)))
		  (with-current-buffer buffer
			(unless (comint-check-proc buffer)
			  (make-comint-in-buffer "pianobar" buffer pianobar-command))
			(comint-send-string buffer (concat username "\n"))
			(comint-send-string buffer (concat password "\n"))
			(pianobar-mode))
		  
		  (set-window-buffer (selected-window) buffer)))))

(provide 'pianobar)
