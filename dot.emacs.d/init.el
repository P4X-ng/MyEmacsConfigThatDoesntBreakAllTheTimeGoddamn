;;; init.el --- Dev friendly emacs --- ;;; Commentary: no.
;;; Commentary: This is also commentary.
;;; I think this works.
;;;
(defvar dynamic "ok" "What is this? We know not.")


;;; Code:
(setq gc-cons-threshold 100000000)

;; --- Bootstrap straight.el ---
(defvar bootstrap-version)
(defvar straight-bootstrap-success nil
  "Track whether straight.el was successfully bootstrapped.")

(condition-case err
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inactive)
          (goto-char (point-max)) (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)
      (setq straight-bootstrap-success t))
  (error 
   (message "Failed to bootstrap straight.el: %s" err)
   (message "Please check your internet connection and try again.")
   (message "Emacs will continue with limited functionality.")
   (setq straight-bootstrap-success nil)))

;; Only proceed with package management if straight.el loaded successfully
(if straight-bootstrap-success
    (progn
      (straight-use-package 'use-package)
      (setq straight-use-package-by-default t))
  ;; Provide a no-op use-package macro if straight.el failed
  ;; Note: We show warnings at startup (not per-package) to ensure users are
  ;; aware of the problem while avoiding message spam from 33+ package declarations
  (progn
    (message "⚠️  WARNING: straight.el not available - packages will not be loaded!")
    (message "⚠️  Emacs will run with limited functionality.")
    (defmacro use-package (_name &rest _args)
      "Fallback no-op use-package when straight.el is not available.
Silently ignores package declarations to avoid console spam."
      nil)))

;; --- Enhanced UI setup ---
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-screen t)

;; --- VSCode-like Theming ---
;; Better defaults for visual appeal
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq ring-bell-function 'ignore)

;; --- Modern Theme ---
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; --- Enhanced Modeline ---
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t
        doom-modeline-irc-stylize 'identity
        doom-modeline-github-timer nil
        doom-modeline-gnus-timer nil))

;; --- Icons (required for doom-modeline) ---
(use-package all-the-icons
  :if (display-graphic-p))

;; --- Terminal & Shell Configuration ---
;; Helper function to determine best available shell type for shell-pop
(defun my/shell-pop-shell-type ()
  "Return shell-pop configuration for the best available shell."
  (cond
   ;; Prefer vterm if available (best terminal emulation)
   ((fboundp 'vterm) 
    '("vterm" "*vterm*" (lambda () (vterm))))
   ;; Fall back to ansi-term if vterm not available
   ((fboundp 'ansi-term)
    '("ansi-term" "*ansi-term*" 
      (lambda () (ansi-term (or (getenv "SHELL") "/bin/bash")))))
   ;; Last resort: eshell (not ideal but works everywhere)
   (t 
    '("eshell" "*eshell*" (lambda () (eshell))))))

;; Helper function to determine best available shell function for direct terminal
(defun my/best-shell-function ()
  "Return a function that opens the best available shell."
  (cond
   ((fboundp 'vterm) 
    (lambda () (vterm)))
   ((fboundp 'ansi-term)
    (lambda () (ansi-term (or (getenv "SHELL") "/bin/bash"))))
   (t 
    (lambda () (eshell)))))

;; Proper terminal emulation with vterm (compile-dependent)
(use-package vterm
  :commands vterm
  :init
  ;; Make vterm installation failures silent
  (setq vterm-install t)
  :config
  (setq vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm %s"
        vterm-kill-buffer-on-exit t)
  ;; Better terminal experience
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (define-key vterm-mode-map (kbd "M-<left>") #'tab-previous)
  (define-key vterm-mode-map (kbd "M-<right>") #'tab-next))

;; Fallback terminal for systems where vterm won't compile
(defun my/terminal ()
  "Launch terminal - vterm if available, ansi-term otherwise."
  (interactive)
  (condition-case err
      (funcall (my/best-shell-function))
    (error 
     (message "Failed to open terminal: %s. Trying eshell..." err)
     (eshell))))

(defun my/terminal-here ()
  "Open terminal in current directory."
  (interactive)
  (let ((default-directory (if buffer-file-name
                               (file-name-directory buffer-file-name)
                             default-directory)))
    (my/terminal)))

(defun my/terminal-project ()
  "Open terminal in project root."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory ".git")
                               (locate-dominating-file default-directory ".project")
                               default-directory)))
    (my/terminal)))

;; Terminal keybindings
(global-set-key (kbd "C-c t") #'my/terminal)
(global-set-key (kbd "C-c T") #'my/terminal-here)
(global-set-key (kbd "C-c M-t") #'my/terminal-project)

;; --- Shell-pop: Quick popup terminal (better UX than eshell) ---
;; Shell-pop provides a quick, toggleable terminal that slides in from the bottom
;; This is a much better alternative to eshell for most use cases
(use-package shell-pop
  :config
  ;; Use shared helper function for consistent shell selection
  (setq shell-pop-shell-type (my/shell-pop-shell-type))
  (setq shell-pop-window-size 30)           ; 30% of frame height
  (setq shell-pop-full-span t)              ; Span full width
  (setq shell-pop-window-position "bottom") ; Popup from bottom
  (setq shell-pop-autocd-to-working-dir t)  ; Auto-cd to current file's directory
  (setq shell-pop-restore-window-configuration t) ; Restore layout on close
  (setq shell-pop-cleanup-buffer-at-process-exit t)) ; Clean up when shell exits

;; Quick shell access keybindings
;; F9 - Quick toggle for popup shell (like in modern IDEs)
;; C-` - Alternative toggle (backtick, common in VS Code)
(global-set-key (kbd "<f9>") #'shell-pop)
(global-set-key (kbd "C-`") #'shell-pop)

;; --- Enhanced File Management ---
(use-package dired-sidebar
  :commands dired-sidebar-toggle-sidebar
  :config
  (setq dired-sidebar-width 30
        dired-sidebar-theme 'icons))

;; Better dired experience
(use-package dired-single
  :after dired
  :config
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
  (define-key dired-mode-map (kbd "^") 'dired-single-up-directory))

;; --- Which-key for keybinding help ---
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → "))

;; --- Tab-bar mode ---
(tab-bar-mode 1)
(global-set-key (kbd "M-<prior>") 'tab-previous)
(global-set-key (kbd "M-<next>")  'tab-next)
(global-set-key (kbd "M-t")       'tab-new)
(global-set-key (kbd "M-w")       'tab-close)

;; --- Window Management ---
(use-package windmove
  :config
  (global-set-key (kbd "M-<left>")  'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>")    'windmove-up)
  (global-set-key (kbd "M-<down>")  'windmove-down))

(global-set-key (kbd "C-|") 'split-window-right)
(global-set-key (kbd "C--") 'split-window-below)

;; --- Enhanced Project Management ---
(use-package projectile 
  :init (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config 
  (setq projectile-project-search-path '("~/Projects" "~")
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien))

;; --- Better Search and Navigation ---
(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap t
        ivy-height 15))

(use-package counsel
  :after ivy
  :init (counsel-mode 1)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-recentf)
         ("C-c g" . counsel-git-grep)
         ("C-s" . swiper)))

(use-package swiper
  :after ivy)

;; --- Git Integration Enhancement ---
(use-package git-gutter
  :init (global-git-gutter-mode 1)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; --- Completion + Orderless ---
;; DUAL COMPLETION SYSTEM: Corfu (modern) + Company (reliable fallback)
;; Both are configured to work together - Corfu is primary, Company is backup

;; Corfu: Modern in-buffer completion popup (auto-shows while typing)
(use-package corfu
  :custom
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-delay 0.2)           ; Show completions after 0.2s
  (corfu-auto-prefix 2)            ; Trigger after 2 characters
  (corfu-cycle t)                  ; Enable cycling for `corfu-next/previous`
  (corfu-preview-current nil)      ; Don't preview current candidate
  (corfu-quit-no-match 'separator) ; Quit on no match except after separator
  :init
  (global-corfu-mode)
  ;; Ctrl+Tab for manual completion trigger (especially useful for C/C++)
  (global-set-key (kbd "<C-tab>") 'completion-at-point))

;; Company-mode: Reliable completion backend (backup system)
;; Company provides robust autocompletion that works everywhere
;; If Corfu has issues, Company will still provide completions
;; NOTE: Company is configured to work alongside Corfu, not compete with it
(use-package company
  :config
  ;; Disable company in programming and text modes where Corfu is preferred
  ;; Company remains available for manual activation with M-/ in all modes
  (setq company-global-modes '(not prog-mode text-mode))  ; Corfu handles these
  (setq company-idle-delay 0.3)              ; Slightly slower than corfu to avoid conflict
  (setq company-minimum-prefix-length 3)     ; Require 3 characters (vs corfu's 2)
  (setq company-selection-wrap-around t)     ; Wrap around when cycling
  (setq company-show-numbers t)              ; Show numbers for quick selection
  (setq company-tooltip-align-annotations t) ; Align annotations to right
  (setq company-require-match nil)           ; Don't require match
  (setq company-dabbrev-downcase nil)        ; Don't downcase completions
  (setq company-dabbrev-ignore-case t)       ; Ignore case when completing
  ;; Company backends - order matters (most specific first)
  (setq company-backends 
        '((company-capf           ; Completion-at-point (works with LSP)
           company-files          ; File path completion
           company-keywords)      ; Language keywords
          (company-dabbrev-code   ; Code words from buffers
           company-dabbrev)))     ; All words from buffers
  ;; Keybindings for company
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; Allow manual activation with M-/ (traditional Emacs completion key)
  (global-set-key (kbd "M-/") #'company-complete))

;; Company-box: Modern UI for company-mode (popup with icons and docs)
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (setq company-box-doc-delay 0.5))

;; Corfu popupinfo: Show documentation popup next to completions
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)) ; Show doc after 0.5s, update after 0.2s
  :config
  (corfu-popupinfo-mode))

;; Cape: Completion At Point Extensions - adds extra completion sources
(use-package cape
  :init
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)  ; Dynamic abbreviations
  (add-to-list 'completion-at-point-functions #'cape-file)     ; File paths
  (add-to-list 'completion-at-point-functions #'cape-keyword)) ; Programming language keywords

;; Kind-icon: Add icons to completion candidates showing their type
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Orderless: Flexible completion matching (space-separated patterns)
(use-package orderless
  :after corfu
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --- Syntax checking + LSP ---
;; Flycheck: Real-time syntax checking
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
    (condition-case err
        (progn
          (lsp-register-client
           (make-lsp-client
            :new-connection (lsp-stdio-connection (lambda () my/jedi-lsp-path))
            :major-modes '(python-mode python-ts-mode)
            :priority 1  ;; Higher priority than pyright (0)
            :server-id 'jedi-lsp
            :initialization-options (lambda () '())
            :initialized-fn (lambda (_workspace)
                              (message "[LSP] jedi-language-server initialized"))))
          (setq my/jedi-lsp-registered t))
      (error (message "[LSP] Failed to register jedi-language-server: %s" err)))))

;; Choose Python LSP: jedi (containerized) > pyright
(defun my/python-lsp-setup ()
  "Setup Python LSP, preferring containerized jedi over pyright."
  (condition-case err
      (progn
        (my/ensure-jedi-lsp-registered)
        (cond
         ((my/jedi-lsp-available-p)
          (message "[LSP] Using containerized jedi-language-server")
          (lsp))
         ((executable-find "pyright")
          (message "[LSP] Using pyright")
          (if (require 'lsp-pyright nil t)
              (lsp)
            (message "[LSP] Failed to load lsp-pyright package")))
         (t (message "[LSP] No Python language server found"))))
    (error (message "[LSP] Python LSP setup failed: %s" err))))

;; LSP-mode: Language Server Protocol for intelligent code features
;; Provides: autocompletion, go-to-definition, find-references, documentation, etc.
;; SETUP NOTES:
;; - C/C++: Install clangd (Ubuntu 24.04: sudo apt install clangd)
;; - Bash: Install bash-language-server with: npm install -g bash-language-server
;; - TypeScript/JavaScript: Install typescript-language-server with: npm install -g typescript-language-server typescript
;; - Python: Jedi is preferred (containerized), pyright as fallback
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet t
        lsp-idle-delay 0.3
        lsp-warn-no-matched-clients nil
        ;; Improve LSP completion integration with corfu
        lsp-completion-provider :none) ; We use corfu, not lsp's built-in completion
  :hook ((python-mode . my/python-lsp-setup)
         (bash-mode . (lambda () (when (executable-find "bash-language-server") (lsp))))
         (sh-mode . (lambda () (when (executable-find "bash-language-server") (lsp))))
         (c-mode . (lambda () (when (executable-find "clangd") (lsp))))
         (c++-mode . (lambda () (when (executable-find "clangd") (lsp))))
         (c-ts-mode . (lambda () (when (executable-find "clangd") (lsp))))
         (c++-ts-mode . (lambda () (when (executable-find "clangd") (lsp))))
         (f90-mode . (lambda () (when (executable-find "fortls") (lsp))))
         (fortran-mode . (lambda () (when (executable-find "fortls") (lsp))))
         (typescript-mode . (lambda () (when (executable-find "typescript-language-server") (lsp))))
         (js2-mode . (lambda () (when (executable-find "typescript-language-server") (lsp))))
         (js-mode . (lambda () (when (executable-find "typescript-language-server") (lsp)))))
  :commands lsp)

;; Configure LSP completion to work seamlessly with corfu
(defun my/lsp-mode-setup-completion ()
  (when (boundp 'completion-category-defaults)
    (let ((lsp-capf-entry (assq 'lsp-capf completion-category-defaults)))
      (if lsp-capf-entry
          (setf (alist-get 'styles (cdr lsp-capf-entry)) '(orderless))
        (push '(lsp-capf (styles orderless)) completion-category-defaults)))))
(add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)

;; --- Jedi Language Server Configuration (optional) ---
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

;; LSP-UI: Enhanced UI features for LSP (sideline info, peek definitions, etc.)
(use-package lsp-ui :after lsp-mode :hook (lsp-mode . lsp-ui-mode))

;; --- Git ---
;; --- C/C++ (Treesitter) ---
(use-package c-ts-mode
  :if (treesit-available-p)
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)))

;; --- Fortran ---
(use-package f90
  :mode (("\\.f90\\'" . f90-mode)
         ("\\.f95\\'" . f90-mode)))

;; --- TypeScript/JavaScript ---
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil))

;; --- pf-mode (Custom) ---
(load (expand-file-name "pf-mode.el" user-emacs-directory))

;; --- Projects (project.el) & Git ---
;; Note: project.el is built-in (C-x p).
(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".git" ".hg")))

(use-package magit :commands magit-status :bind ("C-x g" . magit-status))

;; --- Treemacs ---
;; Simple directory browser on the left - no project management
(use-package treemacs
  :defer t
  :bind (("<f8>" . treemacs))
  :config
  (setq treemacs-width 30
        treemacs-follow-mode nil
        treemacs-filewatch-mode nil
        treemacs-fringe-indicator-mode t
        treemacs-git-mode nil
        treemacs-project-follow-mode nil
        treemacs-project-follow-cleanup nil
        treemacs-collapse-dirs 0
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-show-hidden-files t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-asc
        treemacs-show-cursor nil
        treemacs-eldoc-display t))

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
  "Make HTTP request to IDE server.
Returns the parsed JSON response or signals an error on failure."
  (condition-case err
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
            (json-read))))
    (error
     (signal 'error (list (format "IDE Server request failed: %s" (error-message-string err)))))))

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
            (message "IDE Server starting on %s..." ide-server-url)
            ;; Wait for server to be ready with retries
            (run-with-timer 1 nil #'ide-server-check-ready))
        (message "IDE Server script not found at %s" server-script)))))

(defun ide-server-check-ready ()
  "Check if IDE server is ready and report status."
  (condition-case nil
      (let ((response (ide-server-request "/health" "GET")))
        (when (string= (gethash "status" response) "ok")
          (message "IDE Server ready on %s" ide-server-url)))
    (error (message "IDE Server is starting..."))))

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

;; Auto-start IDE server on Emacs startup (with error protection)
(defun my/safe-ide-server-start ()
  "Safely start IDE server, catching any errors."
  (condition-case err
      (ide-server-start)
    (error (message "IDE Server failed to start: %s" err))))

(add-hook 'emacs-startup-hook #'my/safe-ide-server-start)

;; IDE Server keybindings
(global-set-key (kbd "C-c i c") #'ide-server-chat-send)
(global-set-key (kbd "C-c i a") #'ide-server-context-add)
(global-set-key (kbd "C-c i h") #'ide-server-health)
(global-set-key (kbd "C-c i s") #'ide-server-start)
(global-set-key (kbd "C-c i q") #'ide-server-stop)

;; --- IDE Layout Setup ---
(defun my/setup-ide-layout ()
  "Setup IDE-like layout: Treemacs left, shell bottom."
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
      
      ;; Return focus to main editing window
      (select-window main-window))))

(defun my/open-side-panels ()
  "Auto-open panels on startup - using new IDE layout."
  (when (not noninteractive)
    (condition-case err
        (my/setup-ide-layout)
      (error (message "Failed to setup IDE layout: %s" err)))))

(add-hook 'emacs-startup-hook #'my/open-side-panels)

;; --- Clean dead project entries (built-in project.el + projectile, if present) ---
(defun my/prune-dead-projects ()
  "Drop projects that no longer exist so startup is quiet."
  (require 'seq)
  (when (featurep 'project)
    (when (boundp 'project--list)
      (setq project--list
            (seq-filter (lambda (proj)
                          (let ((dir (car proj)))
                            (and dir (file-directory-p dir))))
                        project--list))
      (when (fboundp 'project--write-project-list)
        (project--write-project-list)))))
;; Removed projectile cleanup code
(add-hook 'emacs-startup-hook #'my/prune-dead-projects)

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
             (or (derived-mode-p 'python-mode 'bash-mode 'sh-mode 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode 'typescript-mode 'js2-mode 'js-mode)))
    (save-excursion
      (let ((file buffer-file-name))
        (cond
         ((derived-mode-p 'python-mode)
          (when (executable-find "black")
            (call-process "black" nil nil nil "--quiet" file)))
         ((derived-mode-p 'bash-mode 'sh-mode)
          (when (executable-find "shfmt")
            (call-process "shfmt" nil nil nil "-w" file)))
         ((derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
          (when (executable-find "clang-format")
            (call-process "clang-format" nil nil nil "-i" file)))
         ((derived-mode-p 'typescript-mode 'js2-mode 'js-mode)
          (when (executable-find "prettier")
            (call-process "prettier" nil nil nil "--config-precedence" "file-override" "--write" file)))))
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

;; --- Enhanced Cheat Sheet ---
(defun my/show-cheatsheet ()
  (interactive)
  (with-output-to-temp-buffer "*Keybindings*"
    (princ "🚀 Enhanced Emacs IDE Keybindings\n")
    (princ "=====================================\n\n")
    (princ "💡 Autocompletion (Corfu primary, Company fallback):\n")
    (princ "  Auto ........... Completions appear while typing (2+ chars, Corfu)\n")
    (princ "  C-TAB .......... Trigger Corfu completion manually (Ctrl+Tab)\n")
    (princ "  M-/ ............ Trigger Company completion (alternative) ⭐ NEW!\n")
    (princ "  TAB ............ Accept/cycle forward through completions\n")
    (princ "  C-n / C-p ...... Next/Previous (Company when active)\n")
    (princ "  S-TAB .......... Cycle backward\n")
    (princ "  RET ............ Insert selected completion\n")
    (princ "  ESC ............ Cancel popup\n")
    (princ "  M-<digit> ...... Quick select (Company when active)\n\n")
    (princ "🖥️  Terminal & Shell (IMPROVED!):\n")
    (princ "  F9 ............. Quick popup shell (shell-pop) ⭐ NEW & BETTER!\n")
    (princ "  C-backtick ..... Alternative popup shell toggle (Ctrl+`) ⭐ NEW!\n")
    (princ "  C-c t .......... Open terminal (vterm/ansi-term/eshell)\n")
    (princ "  C-c T .......... Open terminal in current directory\n")
    (princ "  C-c M-t ........ Open terminal in project root\n")
    (princ "  \n")
    (princ "  💡 TIP: F9 gives you a quick popup shell - much better than eshell!\n")
    (princ "      It slides in from the bottom like modern IDEs.\n\n")
    (princ "LSP Commands (C-c l prefix):\n")
    (princ "  C-c l g g ...... Go to definition\n")
    (princ "  C-c l g r ...... Find references\n")
    (princ "  C-c l r r ...... Rename symbol\n")
    (princ "  C-c l h h ...... Show documentation\n")
    (princ "  C-c l = ........ Format buffer/region\n\n")
    (princ "🗂️  Navigation & Files:\n")
    (princ "  F8 ............. Toggle Treemacs sidebar\n")
    (princ "  C-x C-f ........ Find file (enhanced with counsel)\n")
    (princ "  C-c f .......... Recent files\n")
    (princ "  C-s ............ Search in buffer (swiper)\n")
    (princ "  M-x ............ Command palette (enhanced)\n\n")
    (princ "📑 Tabs & Windows:\n")
    (princ "  M-← / M-→ ...... Switch tabs\n")
    (princ "  M-Arrows ....... Switch window focus\n")
    (princ "  C-| / C-- ...... Split window vertical / horizontal\n")
    (princ "  M-PgUp/PgDn .... Previous/Next tabs\n")
    (princ "  M-t / M-w ...... New / Close tab\n\n")
    (princ "📦 Projects & Git:\n")
    (princ "  C-x p .......... Project prefix (find file, switch project)\n")
    (princ "  C-x g .......... Magit status\n\n")
    (princ "LLM / ChatGPT:\n")
    (princ "  C-c C-g ........ Open GPTel chat\n")
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
    (princ "🐍 Python venvs:\n")
    (princ "  C-c v a ........ Activate venv\n")
    (princ "  C-c v d ........ Deactivate venv\n")
    (princ "  C-c v s ........ Show active venv\n\n")
    (princ "💡 Help & Discovery:\n")
    (princ "  C-k ............ Show this cheat sheet\n")
    (princ "  C-h k .......... Describe key\n")
    (princ "  C-h f .......... Describe function\n")
    (princ "  [Wait 0.5s] .... Which-key popup for available keys\n\n")
    (princ "✨ Quality of Life:\n")
    (princ "  - Modern doom-one theme with enhanced modeline\n")
    (princ "  - Git gutter shows changes in fringe\n")
    (princ "  - Line highlighting and bracket matching\n")
    (princ "  - Smart completion with ivy/counsel/swiper\n")
    (princ "  - Real terminal experience with vterm\n")
    (princ "  - Enhanced project management\n\n")
    (princ "Python LSP (Jedi):\n")
    (princ "  Jedi auto-detected from ~/.venv/jedi/\n")
    (princ "  Run jedi-container/setup-jedi.sh to install\n\n")
    (princ "TypeScript/JavaScript LSP:\n")
    (princ "  TypeScript and JavaScript auto-detected\n")
    (princ "  Run: npm install -g typescript-language-server typescript\n\n")
    (princ "Help:\n")
    (princ "  C-k ............ Show this cheat sheet\n\n")
    (princ "See AUTOCOMPLETE_SETUP.md for language server setup.\n")))
(global-set-key (kbd "C-k") #'my/show-cheatsheet)

;; --- Additional Quality of Life Improvements ---
;; Better buffer management
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (or (mode . python-mode)
                              (mode . c-mode)
                              (mode . c++-mode)
                              (mode . bash-mode)
                              (mode . sh-mode)))
           ("Magit" (name . "^magit"))
           ("Terminal" (or (mode . vterm-mode)
                           (mode . term-mode)
                           (mode . ansi-term-mode)))
           ("Help" (or (name . "^\\*Help\\*")
                       (name . "^\\*Apropos\\*")
                       (name . "^\\*info\\*"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

;; Startup performance improvements
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "✅ Emacs startup optimization complete")))

;; --- Suppress warnings and improve error handling ---
(setq warning-suppress-types '((initialization) (package-cl-lib)))
(setq warning-minimum-level :error)  ; Only show actual errors, not warnings
(setq byte-compile-warnings '(not obsolete))

(message "✅ Enhanced Emacs IDE ready! Press C-k for keybindings cheat sheet.")
