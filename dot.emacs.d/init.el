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

;; --- Jedi Language Server (containerized) ---
;; Path to jedi-language-server installed via jedi-container/setup-jedi.sh
(defvar my/jedi-lsp-path
  (expand-file-name "~/.venv/jedi/bin/jedi-language-server")
  "Path to containerized jedi-language-server executable.")

(defvar my/jedi-lsp-registered nil
  "Track whether jedi-lsp client has been registered.")

(defun my/jedi-lsp-available-p ()
  "Check if containerized jedi-language-server is available."
  (and (file-exists-p my/jedi-lsp-path)
       (file-executable-p my/jedi-lsp-path)))

(defun my/ensure-jedi-lsp-registered ()
  "Register jedi-language-server with lsp-mode if available and not already registered."
  (when (and (my/jedi-lsp-available-p)
             (not my/jedi-lsp-registered))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () my/jedi-lsp-path))
      :major-modes '(python-mode python-ts-mode)
      :priority 1  ;; Higher priority than pyright (0)
      :server-id 'jedi-lsp
      :initialization-options (lambda () '())
      :initialized-fn (lambda (_workspace)
                        (message "[LSP] jedi-language-server initialized"))))
    (setq my/jedi-lsp-registered t)))

;; Choose Python LSP: jedi (containerized) > pyright
(defun my/python-lsp-setup ()
  "Setup Python LSP, preferring containerized jedi over pyright."
  (my/ensure-jedi-lsp-registered)
  (cond
   ((my/jedi-lsp-available-p)
    (message "[LSP] Using containerized jedi-language-server")
    (lsp))
   ((executable-find "pyright")
    (message "[LSP] Using pyright")
    (require 'lsp-pyright)
    (lsp))
   (t (message "[LSP] No Python language server found"))))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l"
              lsp-enable-snippet t
              lsp-idle-delay 0.3
              lsp-warn-no-matched-clients nil)
  :hook ((python-mode . my/python-lsp-setup)
         (bash-mode . (lambda () (when (executable-find "bash-language-server") (lsp))))
         (sh-mode . (lambda () (when (executable-find "bash-language-server") (lsp))))
         (c-mode . (lambda () (when (executable-find "clangd") (lsp))))
         (c++-mode . (lambda () (when (executable-find "clangd") (lsp)))))
  :commands lsp)

(defun my/setup-python-lsp ()
  "Setup Python LSP with Jedi or fallback to pyright."
  (let ((jedi-server (my/find-jedi-language-server))
        (pylsp-server (my/find-pylsp-server)))
    (cond
     ;; Prefer Jedi Language Server
     (jedi-server
      (message "[LSP] Using Jedi Language Server: %s" jedi-server)
      (setq-local lsp-jedi-language-server-command jedi-server)
      (require 'lsp-jedi nil t)
      (lsp))
     ;; Fallback to Python LSP Server with Jedi
     (pylsp-server
      (message "[LSP] Using Python LSP Server: %s" pylsp-server)
      (setq-local lsp-pylsp-server-command pylsp-server)
      (require 'lsp-pylsp nil t)
      (lsp))
     ;; Final fallback to pyright
     ((executable-find "pyright")
      (message "[LSP] Using Pyright fallback")
      (require 'lsp-pyright nil t)
      (lsp))
     (t
      (message "[LSP] No Python language server found. Install Jedi with: ./scripts/deploy-jedi.sh")))))

;; --- Jedi Language Server Configuration ---
(use-package lsp-jedi
  :straight (:host github :repo "fredcamps/lsp-jedi")
  :after lsp-mode
  :config
  (setq lsp-jedi-completion-enabled t
        lsp-jedi-completion-include-params t
        lsp-jedi-diagnostics-enabled t
        lsp-jedi-hover-enabled t
        lsp-jedi-references-enabled t
        lsp-jedi-signature-help-enabled t
        lsp-jedi-symbols-enabled t))

(use-package lsp-ui :after lsp-mode :hook (lsp-mode . lsp-ui-mode))

;; --- Jedi Health Check Command ---
(defun my/jedi-health-check ()
  "Run Jedi health check for current venv."
  (interactive)
  (let ((health-check (or (executable-find "jedi-health-check")
                         (when pyvenv-virtual-env
                           (expand-file-name "bin/jedi-health-check" pyvenv-virtual-env))
                         (expand-file-name "jedi/bin/health-check.py" 
                                          (or pyvenv-virtual-env default-directory)))))
    (if (and health-check (file-executable-p health-check))
        (async-shell-command health-check "*Jedi Health Check*")
      (message "Jedi health check not found. Deploy Jedi first: ./scripts/deploy-jedi.sh"))))

(global-set-key (kbd "C-c j h") #'my/jedi-health-check)

;; --- Git ---
(use-package magit :commands magit-status :bind ("C-x g" . magit-status))

;; --- Vterm (real terminal emulator) ---
(use-package vterm
  :commands vterm
  :bind (("C-c t" . vterm)
         ("C-c T" . vterm-other-window))
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

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

;; --- Auto-open panels on startup (treemacs + GPTel chat) ---
(defun my/open-side-panels ()
  (when (not noninteractive)
    (when (fboundp 'treemacs)
      (ignore-errors (treemacs)))
    (when (and (fboundp 'gptel)
               (or (bound-and-true-p gptel-api-key)
                   (getenv "OPENAI_API_KEY")))
      (ignore-errors (gptel)))))
(add-hook 'emacs-startup-hook #'my/open-side-panels)

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
    (princ "  M-t / M-w ...... New / Close tab\n\n")
    (princ "Git:\n")
    (princ "  C-x g .......... Magit status\n\n")
    (princ "LLM / ChatGPT:\n")
    (princ "  C-c g .......... Open GPTel chat\n")
    (princ "  C-c RET ........ Send prompt (inside chat buffer)\n\n")
    (princ "Context Helpers:\n")
    (princ "  C-c s .......... Search & insert from context dirs\n")
    (princ "  C-c r a ........ Add context dir\n")
    (princ "  C-c r r ........ Remove context dir\n\n")
    (princ "Python venvs:\n")
    (princ "  C-c v a ........ Activate\n")
    (princ "  C-c v d ........ Deactivate\n")
    (princ "  C-c v s ........ Show active venv\n\n")
    (princ "Python LSP (Jedi):\n")
    (princ "  Jedi auto-detected from ~/.venv/jedi/\n")
    (princ "  Run jedi-container/setup-jedi.sh to install\n\n")
    (princ "Help:\n")
    (princ "  C-k ............ Show this cheat sheet\n")))
(global-set-key (kbd "C-k") #'my/show-cheatsheet)

;; --- Suppress end-of-file warnings ---
(setq warning-suppress-types '((initialization)))

(message "✅ Emacs IDE ready (terminal-safe).")
