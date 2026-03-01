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
;; Proper terminal emulation with vterm (compile-dependent)
(use-package vterm
  :commands vterm
  :init
  ;; Make vterm installation failures silent
  (setq vterm-install t)
  :config
  (setq vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm %s"
        vterm-kill-buffer-on-exit t
        vterm-clear-scrollback-when-clearing t
        vterm-timer-delay 0.01
        vterm-shell (or (getenv "SHELL") "/bin/bash"))
  
  ;; Better terminal experience with keybindings
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (define-key vterm-mode-map (kbd "M-<left>") #'tab-previous)
  (define-key vterm-mode-map (kbd "M-<right>") #'tab-next)
  (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
  (define-key vterm-mode-map (kbd "M-y") #'vterm-yank-pop)
  
  ;; Prevent prompt overwriting by disabling certain modes in vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (setq-local scroll-margin 0)))
  
  ;; Make vterm buffers more recognizable
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq mode-line-format
                    (list "  "
                          mode-line-front-space
                          "🖥️ VTERM: "
                          'default-directory
                          "  "
                          mode-line-end-spaces)))))

;; Fallback terminal for systems where vterm won't compile
(defun my/terminal ()
  "Launch terminal - vterm if available, ansi-term otherwise."
  (interactive)
  (condition-case err
      (if (fboundp 'vterm)
          (vterm)
        (ansi-term (or (getenv "SHELL") "/bin/bash")))
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

;; Improve eshell as a fallback
(use-package eshell
  :config
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  
  ;; Better prompt
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "green"))
           (propertize " λ " 'face '(:foreground "cyan")))))
  (setq eshell-prompt-regexp "^[^λ]* λ "))

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
  (setq which-key-idle-delay 0.3          ; Show faster (was 0.5)
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.35  ; Show more keys (was 0.25)
        which-key-max-description-length 35    ; Longer descriptions (was 25)
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → "
        which-key-add-column-padding 1
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-compute-remaps t
        which-key-use-C-h-commands t)
  
  ;; Add prefix descriptions for better discoverability
  (which-key-add-key-based-replacements
    "C-c l" "LSP"
    "C-c p" "Project"
    "C-c v" "Python-venv"
    "C-c i" "IDE-server"
    "C-c r" "Context"
    "C-c C-g" "GPTel"
    "C-x g" "Magit"
    "C-x p" "Project.el"))

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
;; Corfu: Modern in-buffer completion popup (auto-shows while typing)
(use-package corfu
  :custom
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-delay 0.1)           ; Show completions faster (was 0.2s)
  (corfu-auto-prefix 1)            ; Trigger after 1 character (was 2)
  (corfu-cycle t)                  ; Enable cycling for `corfu-next/previous`
  (corfu-preview-current 'insert)  ; Show preview of current candidate
  (corfu-quit-no-match 'separator) ; Quit on no match except after separator
  (corfu-preselect 'prompt)        ; Preselect first candidate
  (corfu-on-exact-match nil)       ; Don't auto-insert exact matches
  (corfu-min-width 20)
  (corfu-max-width 100)
  (corfu-count 10)                 ; Show more candidates
  :init
  (global-corfu-mode)
  ;; Better completion keybindings
  (global-set-key (kbd "<C-tab>") 'completion-at-point)
  (global-set-key (kbd "M-/") 'completion-at-point)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location)))

;; Corfu popupinfo: Show documentation popup next to completions
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.3 . 0.1)) ; Show doc faster (was 0.5, 0.2)
  (corfu-popupinfo-max-width 80)
  (corfu-popupinfo-max-height 20)
  :config
  (corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)))

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
(use-package flycheck
  :init (global-flycheck-mode 1)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.5
        flycheck-display-errors-delay 0.3
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-checker-error-threshold 1000)
  
  ;; Better error display in echo area
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  
  ;; Define custom fringe indicators for better visibility
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  
  ;; Show errors in modeline
  (setq flycheck-mode-line-prefix "✓")
  
  :bind (("C-c ! l" . flycheck-list-errors)
         ("C-c ! n" . flycheck-next-error)
         ("C-c ! p" . flycheck-previous-error)
         ("C-c ! v" . flycheck-verify-setup)))

;; Flycheck inline display - show errors right in the buffer
(use-package flycheck-inline
  :after flycheck
  :config
  (global-flycheck-inline-mode))

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
            :priority 2  ;; Higher priority than pyright (0) and lsp-jedi (1)
            :server-id 'jedi-lsp-containerized
            :initialization-options (lambda () '())
            :initialized-fn (lambda (_workspace)
                              (message "✓ [LSP] jedi-language-server (containerized) initialized successfully"))))
          (setq my/jedi-lsp-registered t)
          (message "✓ [LSP] jedi-language-server client registered"))
      (error (message "✗ [LSP] Failed to register jedi-language-server: %s" err)))))

;; Choose Python LSP: jedi (containerized) > pyright
(defun my/python-lsp-setup ()
  "Setup Python LSP, preferring containerized jedi over pyright."
  (condition-case err
      (progn
        (my/ensure-jedi-lsp-registered)
        (cond
         ((my/jedi-lsp-available-p)
          (message "✓ [LSP] Using containerized jedi-language-server for Python")
          (lsp))
         ((executable-find "pyright")
          (message "ℹ [LSP] Using pyright for Python (jedi not found)")
          (if (require 'lsp-pyright nil t)
              (lsp)
            (message "✗ [LSP] Failed to load lsp-pyright package")))
         (t 
          (message "⚠ [LSP] No Python language server found!")
          (message "  Install jedi: Run jedi-container/setup-jedi.sh")
          (message "  Or install pyright: pip install pyright"))))
    (error (message "✗ [LSP] Python LSP setup failed: %s" err))))

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
        lsp-idle-delay 0.2                    ; Faster LSP response (was 0.3)
        lsp-warn-no-matched-clients nil
        lsp-completion-provider :none         ; We use corfu
        lsp-headerline-breadcrumb-enable t    ; Show breadcrumb
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-modeline-code-actions-enable t    ; Show code actions in modeline
        lsp-modeline-diagnostics-enable t     ; Show errors/warnings count
        lsp-signature-auto-activate t         ; Show signature help
        lsp-signature-render-documentation t
        lsp-eldoc-enable-hover t
        lsp-diagnostics-provider :flycheck)   ; Use flycheck for diagnostics
  :hook ((python-mode . my/python-lsp-setup)
         (bash-mode . (lambda () 
                        (when (executable-find "bash-language-server") 
                          (message "✓ [LSP] Starting bash-language-server")
                          (lsp))))
         (sh-mode . (lambda () 
                      (when (executable-find "bash-language-server") 
                        (message "✓ [LSP] Starting bash-language-server")
                        (lsp))))
         (c-mode . (lambda () 
                     (when (executable-find "clangd") 
                       (message "✓ [LSP] Starting clangd for C")
                       (lsp))))
         (c++-mode . (lambda () 
                       (when (executable-find "clangd") 
                         (message "✓ [LSP] Starting clangd for C++")
                         (lsp))))
         (c-ts-mode . (lambda () 
                        (when (executable-find "clangd") 
                          (message "✓ [LSP] Starting clangd for C")
                          (lsp))))
         (c++-ts-mode . (lambda () 
                          (when (executable-find "clangd") 
                            (message "✓ [LSP] Starting clangd for C++")
                            (lsp))))
         (f90-mode . (lambda () 
                       (when (executable-find "fortls") 
                         (message "✓ [LSP] Starting fortls")
                         (lsp))))
         (fortran-mode . (lambda () 
                           (when (executable-find "fortls") 
                             (message "✓ [LSP] Starting fortls")
                             (lsp))))
         (typescript-mode . (lambda () 
                              (when (executable-find "typescript-language-server") 
                                (message "✓ [LSP] Starting typescript-language-server")
                                (lsp))))
         (js2-mode . (lambda () 
                       (when (executable-find "typescript-language-server") 
                         (message "✓ [LSP] Starting typescript-language-server")
                         (lsp))))
         (js-mode . (lambda () 
                      (when (executable-find "typescript-language-server") 
                        (message "✓ [LSP] Starting typescript-language-server")
                        (lsp)))))
  :commands lsp
  :bind (:map lsp-mode-map
              ("C-c l f" . lsp-format-buffer)
              ("C-c l r" . lsp-rename)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l i" . lsp-find-implementation)))

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
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-delay 0.5
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t)
  :bind (:map lsp-ui-mode-map
              ("C-c l ." . lsp-ui-peek-find-definitions)
              ("C-c l ?" . lsp-ui-peek-find-references)
              ("C-c l D" . lsp-ui-doc-show)))

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
    (princ "Emacs IDE Keybindings\n=====================\n\n")
    (princ "Autocompletion:\n")
    (princ "  Auto ........... Completions appear while typing (2+ chars)\n")
    (princ "  C-TAB .......... Trigger completion manually (Ctrl+Tab)\n")
    (princ "  TAB ............ Accept/cycle forward through completions\n")
    (princ "  S-TAB .......... Cycle backward\n")
    (princ "  RET ............ Insert selected completion\n")
    (princ "  ESC ............ Cancel popup\n\n")
    (princ "LSP Commands (C-c l prefix):\n")
    (princ "  C-c l g g ...... Go to definition\n")
    (princ "  C-c l g r ...... Find references\n")
    (princ "  C-c l r r ...... Rename symbol\n")
    (princ "  C-c l h h ...... Show documentation\n")
    (princ "  C-c l = ........ Format buffer/region\n\n")
    (princ "Navigation / UI:\n")
    (princ "🚀 Enhanced Emacs IDE Keybindings\n")
    (princ "=====================================\n\n")
    (princ "🖥️  Terminal & Shell:\n")
    (princ "  C-c t .......... Open terminal (vterm/ansi-term)\n")
    (princ "  C-c T .......... Open terminal in current directory\n")
    (princ "  C-c M-t ........ Open terminal in project root\n\n")
    (princ "🗂️  Navigation & Files:\n")
    (princ "  F8 ............. Toggle Treemacs sidebar\n")
    (princ "  C-x C-f ........ Find file (enhanced with counsel)\n")
    (princ "  C-c f .......... Recent files\n")
    (princ "  C-s ............ Search in buffer (swiper)\n")
    (princ "  M-x ............ Command palette (enhanced)\n\n")
    (princ "📑 Tabs & Windows:\n")
    (princ "  M-← / M-→ ...... Switch tabs\n")
    (princ "  M-t / M-w ...... New / Close tab\n")
    (princ "  C-c l .......... Reset IDE layout\n\n")
    (princ "Git:\n")
    (princ "  M-Arrows ....... Switch window focus\n")
    (princ "  C-| / C-- ...... Split window vertical / horizontal\n")
    (princ "  M-PgUp/PgDn .... Switch tabs\n")
    (princ "  M-t / M-w ...... New / Close tab\n\n")
    (princ "Projects & Git:\n")
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
;; Add hydra for common command discovery
(use-package hydra
  :config
  (defhydra hydra-main-menu (:color blue :hint nil)
    "
^Navigation^        ^Editing^           ^LSP/Code^          ^Git^           ^Help^
^^^^^^^^---------------------------------------------------------------------------
_f_: Find file      _s_: Search         _L_: LSP menu       _g_: Magit      _?_: Keybindings
_b_: Switch buffer  _R_: Replace        _c_: Completion     _D_: Diff       _h_: Describe key
_t_: Toggle tree    _u_: Undo tree      _e_: Errors list    _B_: Blame      _i_: Info
_p_: Project        _m_: Multiple cur   _d_: Definition     _l_: Log        _a_: Apropos
_T_: Terminal       _/_: Comment        _r_: References     ^ ^             ^ ^
"
    ("f" counsel-find-file)
    ("b" counsel-switch-buffer)
    ("t" treemacs)
    ("p" projectile-command-map)
    ("T" my/terminal)
    ("s" swiper)
    ("R" query-replace)
    ("u" undo-tree-visualize)
    ("m" mc/edit-lines)
    ("/" comment-line)
    ("L" lsp-command-map :color red)
    ("c" completion-at-point)
    ("e" flycheck-list-errors)
    ("d" lsp-find-definition)
    ("r" lsp-find-references)
    ("g" magit-status)
    ("D" magit-diff)
    ("B" magit-blame)
    ("l" magit-log)
    ("?" my/show-cheatsheet)
    ("h" describe-key)
    ("i" info)
    ("a" apropos)
    ("q" nil "quit" :color blue))
  
  (global-set-key (kbd "C-c m") 'hydra-main-menu/body)
  (global-set-key (kbd "<f1>") 'hydra-main-menu/body))

;; Function to list all custom keybindings
(defun my/describe-personal-keybindings ()
  "Display all custom keybindings defined in this configuration."
  (interactive)
  (with-output-to-temp-buffer "*Personal Keybindings*"
    (princ "Personal Keybindings\n")
    (princ "====================\n\n")
    (princ "Press F1 or C-c m for interactive command menu (Hydra)\n\n")
    (princ "COMPLETION & CODE:\n")
    (princ "  C-TAB / M-/           Trigger completion manually\n")
    (princ "  TAB                   Accept/next completion\n")
    (princ "  S-TAB                 Previous completion\n")
    (princ "  M-d                   Show documentation (in completion)\n")
    (princ "  M-l                   Show location (in completion)\n\n")
    (princ "LSP COMMANDS (C-c l prefix):\n")
    (princ "  C-c l g g / C-c l .   Go to definition\n")
    (princ "  C-c l g r / C-c l ?   Find references\n")
    (princ "  C-c l r / C-c l r r   Rename symbol\n")
    (princ "  C-c l f / C-c l =     Format buffer\n")
    (princ "  C-c l d / C-c l D     Show documentation\n")
    (princ "  C-c l a               Code actions\n")
    (princ "  C-c l i               Find implementation\n\n")
    (princ "SYNTAX CHECKING (C-c ! prefix):\n")
    (princ "  C-c ! l               List errors\n")
    (princ "  C-c ! n               Next error\n")
    (princ "  C-c ! p               Previous error\n")
    (princ "  C-c ! v               Verify setup\n\n")
    (princ "TERMINAL:\n")
    (princ "  C-c t                 Open terminal\n")
    (princ "  C-c T                 Terminal in current dir\n")
    (princ "  C-c M-t               Terminal in project root\n\n")
    (princ "NAVIGATION:\n")
    (princ "  F8                    Toggle Treemacs\n")
    (princ "  C-x C-f               Find file (counsel)\n")
    (princ "  C-c f                 Recent files\n")
    (princ "  C-s                   Search (swiper)\n")
    (princ "  M-x                   Command palette\n\n")
    (princ "TABS & WINDOWS:\n")
    (princ "  M-← / M-→             Switch tabs\n")
    (princ "  M-t / M-w             New / Close tab\n")
    (princ "  M-arrows              Move between windows\n")
    (princ "  C-| / C--             Split window vert/horiz\n\n")
    (princ "GIT:\n")
    (princ "  C-x g                 Magit status\n\n")
    (princ "HELP:\n")
    (princ "  C-k                   Full cheat sheet\n")
    (princ "  F1 / C-c m            Interactive menu (Hydra)\n")
    (princ "  C-h k                 Describe key\n")
    (princ "  C-h f                 Describe function\n\n")
    (princ "See C-k for the complete cheat sheet.\n")))

(global-set-key (kbd "C-h K") 'my/describe-personal-keybindings)

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

;; Enhanced startup message
(defun my/show-startup-info ()
  "Display helpful startup information."
  (let ((lsp-servers '())
        (missing-servers '()))
    ;; Check for language servers
    (when (executable-find "clangd")
      (push "C/C++ (clangd)" lsp-servers))
    (unless (executable-find "clangd")
      (push "C/C++ (install: sudo apt install clangd)" missing-servers))
    
    (when (executable-find "bash-language-server")
      (push "Bash (bash-language-server)" lsp-servers))
    (unless (executable-find "bash-language-server")
      (push "Bash (install: npm install -g bash-language-server)" missing-servers))
    
    (when (executable-find "typescript-language-server")
      (push "TypeScript/JS (typescript-language-server)" lsp-servers))
    (unless (executable-find "typescript-language-server")
      (push "TypeScript/JS (install: npm install -g typescript-language-server typescript)" missing-servers))
    
    (when (my/jedi-lsp-available-p)
      (push "Python (Jedi - containerized)" lsp-servers))
    (when (and (not (my/jedi-lsp-available-p)) (executable-find "pyright"))
      (push "Python (pyright)" lsp-servers))
    (when (and (not (my/jedi-lsp-available-p)) (not (executable-find "pyright")))
      (push "Python (install jedi: see jedi-container/setup-jedi.sh or pip install pyright)" missing-servers))
    
    (message "========================================")
    (message "✅ Enhanced Emacs IDE Ready!")
    (message "========================================")
    (message "Press F1 or C-c m for interactive command menu")
    (message "Press C-k for full keybindings cheat sheet")
    (message "Press C-h K for personal keybindings list")
    (message "")
    (when lsp-servers
      (message "✓ Available LSP servers:")
      (dolist (server lsp-servers)
        (message "  • %s" server)))
    (when missing-servers
      (message "")
      (message "ℹ Missing LSP servers (optional):")
      (dolist (server missing-servers)
        (message "  ○ %s" server)))
    (message "")
    (message "Features:")
    (message "  • Auto-completion (Corfu) - triggers after 1 char")
    (message "  • Syntax checking (Flycheck) - real-time errors")
    (message "  • Enhanced terminal (vterm) - C-c t to open")
    (message "  • File explorer (Treemacs) - F8 to toggle")
    (message "  • Git integration (Magit) - C-x g")
    (message "  • Which-key hints - wait 0.3s after prefix")
    (message "========================================")))

(add-hook 'emacs-startup-hook #'my/show-startup-info)

(message "✅ Enhanced Emacs IDE configuration loaded! Press C-k for keybindings.")
