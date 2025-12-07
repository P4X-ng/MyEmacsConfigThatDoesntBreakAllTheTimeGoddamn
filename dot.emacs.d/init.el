;;; init.el --- Dev friendly emacs --- ;;; Commentary: no.
;;; Commentary: This is also commentary.
;;; I think this works.
;;;
(defvar dynamic "ok" "What is this? We know not.")


;;; Code:
(setq gc-cons-threshold 100000000)

;; --- Bootstrap straight.el ---
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inactive)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; --- Safe UI setup (terminal-friendly) ---
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-screen t)

;; --- Tab-bar mode ---
(tab-bar-mode 1)
(global-set-key (kbd "M-<left>") 'tab-previous)
(global-set-key (kbd "M-<right>") 'tab-next)
(global-set-key (kbd "M-t") 'tab-new)
(global-set-key (kbd "M-w") 'tab-close)

;; --- Completion + Orderless ---
(use-package corfu :init (global-corfu-mode))
(use-package orderless
  :after corfu
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --- Syntax checking + LSP ---
(use-package flycheck :init (global-flycheck-mode 1))
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l"
              lsp-enable-snippet t
              lsp-idle-delay 0.3
              lsp-warn-no-matched-clients nil)
  :hook ((python-mode . (lambda () (when (executable-find "pyright") (lsp))))
         (bash-mode . (lambda () (when (executable-find "bash-language-server") (lsp))))
         (sh-mode . (lambda () (when (executable-find "bash-language-server") (lsp))))
         (c-mode . (lambda () (when (executable-find "clangd") (lsp))))
         (c++-mode . (lambda () (when (executable-find "clangd") (lsp)))))
  :commands lsp)
(use-package lsp-ui :after lsp-mode :hook (lsp-mode . lsp-ui-mode))
(use-package lsp-pyright :after lsp-mode
  :hook (python-mode . (lambda () (when (executable-find "pyright") (require 'lsp-pyright) (lsp)))))

;; --- Git ---
(use-package magit :commands magit-status :bind ("C-x g" . magit-status))

;; --- Treemacs ---
(use-package treemacs
  :defer t
  :bind (("<f8>" . treemacs))
  :config (setq treemacs-width 30))

;; --- GPTel (Chat / LLM) ---
(use-package gptel
  :init
  (setq gptel-default-model "gpt-4o-mini")
  (cond
   ((string= (or (getenv "GPTEL_BACKEND") "") "vllm")
    (setq gptel-openai-base-url "http://localhost:8000/v1"
          gptel-api-key (or (getenv "OPENAI_API_KEY") "local-llm-token")))
   ((string= (or (getenv "GPTEL_BACKEND") "") "tgi")
    (setq gptel-openai-base-url "http://localhost:8080/v1"
          gptel-api-key (or (getenv "OPENAI_API_KEY") "local-llm-token"))))
  :config
  (global-set-key (kbd "C-c C-g") #'gptel))

;; --- IDE Server Integration ---
(defvar ide-server-url "http://127.0.0.1:9999"
  "Base URL for the Python IDE server.")

(defvar ide-server-process nil
  "Process running the IDE server.")

(defun ide-server-request (endpoint method &optional data)
  "Make HTTP request to IDE server."
  (let ((url (concat ide-server-url endpoint))
        (url-request-method method)
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (when data (json-encode data))))
    (with-current-buffer (url-retrieve-synchronously url t nil 5)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read)))))

(defun ide-server-start ()
  "Start the IDE server if not already running."
  (interactive)
  (unless (and ide-server-process (process-live-p ide-server-process))
    (let* ((server-dir (expand-file-name "ide-server" user-emacs-directory))
           (server-script (expand-file-name "server.py" server-dir)))
      (if (file-exists-p server-script)
          (progn
            (setq ide-server-process
                  (start-process "ide-server" "*IDE Server*"
                                 "python3" "-u" server-script))
            (message "IDE Server started on %s" ide-server-url)
            ;; Give it a moment to start up
            (sleep-for 2))
        (message "IDE Server script not found at %s" server-script)))))

(defun ide-server-stop ()
  "Stop the IDE server."
  (interactive)
  (when (and ide-server-process (process-live-p ide-server-process))
    (kill-process ide-server-process)
    (setq ide-server-process nil)
    (message "IDE Server stopped")))

(defun ide-server-health ()
  "Check IDE server health."
  (interactive)
  (condition-case err
      (let ((response (ide-server-request "/health" "GET")))
        (message "IDE Server status: %s" (gethash "status" response)))
    (error (message "IDE Server not responding: %s" err))))

(defun ide-server-chat-send (message)
  "Send MESSAGE to IDE server chat."
  (interactive "sMessage: ")
  (condition-case err
      (let* ((data (list (cons "message" message)))
             (response (ide-server-request "/chat/send" "POST" data))
             (reply (gethash "response" response)))
        (message "IDE Chat: %s" reply)
        (with-current-buffer (get-buffer-create "*IDE Chat*")
          (goto-char (point-max))
          (insert (format "\nYou: %s\n" message))
          (insert (format "Assistant: %s\n" reply))
          (display-buffer (current-buffer))))
    (error (message "Failed to send chat message: %s" err))))

(defun ide-server-context-add (path)
  "Add PATH to IDE server context directories."
  (interactive "DAdd context directory: ")
  (condition-case err
      (let* ((data (list (cons "path" (expand-file-name path))))
             (response (ide-server-request "/context/add" "POST" data)))
        (if (eq (gethash "success" response) t)
            (message "Added context directory: %s" path)
          (message "Failed to add context directory")))
    (error (message "Failed to add context directory: %s" err))))

;; Auto-start IDE server on Emacs startup
(add-hook 'emacs-startup-hook #'ide-server-start)

;; IDE Server keybindings
(global-set-key (kbd "C-c i c") #'ide-server-chat-send)
(global-set-key (kbd "C-c i a") #'ide-server-context-add)
(global-set-key (kbd "C-c i h") #'ide-server-health)
(global-set-key (kbd "C-c i s") #'ide-server-start)
(global-set-key (kbd "C-c i q") #'ide-server-stop)

;; --- IDE Layout Setup ---
(defun my/setup-ide-layout ()
  "Setup IDE-like layout: Treemacs left, shell bottom, chat right."
  (interactive)
  (when (not noninteractive)
    ;; Delete other windows first
    (delete-other-windows)
    
    ;; Open treemacs on the left (it manages its own window)
    (when (fboundp 'treemacs)
      (ignore-errors (treemacs)))
    
    ;; Split for shell at bottom (30% height)
    (let* ((main-window (selected-window))
           (shell-window (split-window main-window nil 'below))
           (shell-height (floor (* 0.3 (window-total-height)))))
      (select-window shell-window)
      (window-resize shell-window (- shell-height (window-total-height)))
      ;; Open shell/eshell in bottom window
      (if (fboundp 'vterm)
          (vterm)
        (eshell))
      
      ;; Go back to main window and split right for chat (30% width)
      (select-window main-window)
      (let* ((chat-window (split-window main-window nil 'right))
             (chat-width (floor (* 0.3 (window-total-width)))))
        (select-window chat-window)
        (window-resize chat-window (- chat-width (window-total-width)) t)
        ;; Open IDE chat or GPTel in right window
        (if (get-buffer "*IDE Chat*")
            (switch-to-buffer "*IDE Chat*")
          (when (and (fboundp 'gptel)
                     (or (bound-and-true-p gptel-api-key)
                         (getenv "OPENAI_API_KEY")))
            (ignore-errors (gptel))))
        
        ;; Return focus to main editing window
        (select-window main-window)))))

(defun my/open-side-panels ()
  "Auto-open panels on startup - using new IDE layout."
  (when (not noninteractive)
    (my/setup-ide-layout)))

(add-hook 'emacs-startup-hook #'my/open-side-panels)

;; Keybinding to reset IDE layout
(global-set-key (kbd "C-c l") #'my/setup-ide-layout)



(defun my/cleanup-treemacs-persist ()
  "Nuke treemacs cache if it points at missing paths."
  (let ((persist (expand-file-name ".cache/treemacs-persist" user-emacs-directory)))
    (when (file-exists-p persist)
      (with-temp-buffer
        (insert-file-contents persist)
        (goto-char (point-min))
        (let (bad)
          (while (re-search-forward "- path :: \\(.*\\)" nil t)
            (let* ((raw (match-string 1))
                   (path (string-trim (substitute-in-file-name raw))))
              (unless (file-directory-p path)
                (setq bad t))))
          (when bad
            (ignore-errors
              (delete-file persist)
              (let ((bak (concat persist "~")))
                (when (file-exists-p bak) (delete-file bak)))))))))
  (when (fboundp 'treemacs)
    (ignore-errors (treemacs))))
(add-hook 'emacs-startup-hook #'my/cleanup-treemacs-persist)

;; Replace the whole auto-format section with this:

(defun my/format-buffer-after-save ()
  (when (and buffer-file-name
             (or (derived-mode-p 'python-mode 'bash-mode 'sh-mode 'c-mode 'c++-mode)))
    (save-excursion
      (let ((file buffer-file-name))
        (cond
         ((derived-mode-p 'python-mode)
          (when (executable-find "black")
            (call-process "black" nil nil nil "--quiet" file)))
         ((derived-mode-p 'bash-mode 'sh-mode)
          (when (executable-find "shfmt")
            (call-process "shfmt" nil nil nil "-w" file)))
         ((derived-mode-p 'c-mode 'c++-mode)
          (when (executable-find "clang-format")
            (call-process "clang-format" nil nil nil "-i" file)))))
      ;; Reload the now-formatted file without prompts
      (revert-buffer :ignore-auto :noconfirm))))

;; Remove this if it exists:
;; (add-hook 'before-save-hook #'my/format-buffer-on-save)

(add-hook 'after-save-hook #'my/format-buffer-after-save)

;; --- Python venv auto-detection ---
(use-package pyvenv
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/.venv/"))
  (python-mode)
  (defun my/auto-venv ()
    (let* ((root (or (locate-dominating-file default-directory ".venv")
                     (locate-dominating-file default-directory "venv"))))
      (when root
        (let ((path (expand-file-name (if (file-directory-p (concat root ".venv"))
                                          ".venv" "venv") root)))
          (when (file-exists-p (expand-file-name "bin/python" path))
            (message "[venv] activating %s" path)
            (pyvenv-activate path))))))
  :hook (python-mode . my/auto-venv))
(global-set-key (kbd "C-c v a") #'pyvenv-activate)
(global-set-key (kbd "C-c v d") #'pyvenv-deactivate)
(global-set-key (kbd "C-c v s") (lambda () (interactive)
  (message "Active venv: %s" (or pyvenv-virtual-env "none"))))

;; --- Context Search / Insert ---
(defvar my/context-roots (list (expand-file-name "~/.llm-context")))
(defun my/add-context-root (dir)
  (interactive "DAdd context dir: ")
  (push (directory-file-name dir) my/context-roots)
  (message "Added: %s" dir))
(defun my/remove-context-root (dir)
  (interactive (list (completing-read "Remove context dir: " my/context-roots)))
  (setq my/context-roots (remove dir my/context-roots))
  (message "Removed: %s" dir))
(defun my/search-context (query)
  (interactive "sSearch context for: ")
  (let* ((roots (mapconcat 'shell-quote-argument my/context-roots " "))
         (cmd (format "rg -n -C2 -m3 -F %s %s" (shell-quote-argument query) roots))
         (out (shell-command-to-string cmd)))
    (if (string-empty-p out)
        (message "No context found.")
      (insert (format "\n# CONTEXT: %s\n%s\n" query out)))))
(global-set-key (kbd "C-c r a") #'my/add-context-root)
(global-set-key (kbd "C-c r r") #'my/remove-context-root)
(global-set-key (kbd "C-c s") #'my/search-context)

;; --- Cheat Sheet ---
(defun my/show-cheatsheet ()
  (interactive)
  (with-output-to-temp-buffer "*Keybindings*"
    (princ "Emacs IDE Keybindings\n=====================\n\n")
    (princ "Navigation / UI:\n")
    (princ "  F8 ............. Toggle Treemacs sidebar\n")
    (princ "  M-← / M-→ ...... Switch tabs\n")
    (princ "  M-t / M-w ...... New / Close tab\n")
    (princ "  C-c l .......... Reset IDE layout\n\n")
    (princ "Git:\n")
    (princ "  C-x g .......... Magit status\n\n")
    (princ "LLM / ChatGPT:\n")
    (princ "  C-c g .......... Open GPTel chat\n")
    (princ "  C-c RET ........ Send prompt (inside chat buffer)\n\n")
    (princ "IDE Server (Python):\n")
    (princ "  C-c i c ........ Send chat message to IDE server\n")
    (princ "  C-c i a ........ Add context directory\n")
    (princ "  C-c i h ........ Check IDE server health\n")
    (princ "  C-c i s ........ Start IDE server\n")
    (princ "  C-c i q ........ Stop IDE server\n\n")
    (princ "Context Helpers:\n")
    (princ "  C-c s .......... Search & insert from context dirs\n")
    (princ "  C-c r a ........ Add context dir\n")
    (princ "  C-c r r ........ Remove context dir\n\n")
    (princ "Python venvs:\n")
    (princ "  C-c v a ........ Activate\n")
    (princ "  C-c v d ........ Deactivate\n")
    (princ "  C-c v s ........ Show active venv\n\n")
    (princ "Help:\n")
    (princ "  C-k ............ Show this cheat sheet\n")))
(global-set-key (kbd "C-k") #'my/show-cheatsheet)

;; --- Suppress end-of-file warnings ---
(setq warning-suppress-types '((initialization)))

(message "✅ Emacs IDE ready (terminal-safe).")
