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

;; --- Enhanced UI setup ---
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-screen t)

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
  (if (fboundp 'vterm)
      (vterm)
    (ansi-term (or (getenv "SHELL") "/bin/bash"))))

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
(global-set-key (kbd "M-<left>") 'tab-previous)
(global-set-key (kbd "M-<right>") 'tab-next)
(global-set-key (kbd "M-t") 'tab-new)
(global-set-key (kbd "M-w") 'tab-close)

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

;; --- Projects & Git ---
;; (use-package projectile :init (projectile-mode 1)
;;  :bind-keymap ("C-c p" . projectile-command-map)
;;  :config (setq projectile-project-search-path '("~/Projects" "~")))
(use-package magit :commands magit-status :bind ("C-x g" . magit-status))

;; --- Treemacs ---
(use-package treemacs
  :defer t
  :bind (("<f8>" . treemacs))
  :config (setq treemacs-width 30))
(use-package treemacs-projectile :after (treemacs projectile))

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
        (project--write-project-list))))
  (when (featurep 'projectile)
    (when (boundp 'projectile-known-projects)
      (setq projectile-known-projects
            (seq-filter #'file-directory-p projectile-known-projects))
      (when (fboundp 'projectile-save-known-projects)
        (projectile-save-known-projects)))))
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

;; --- Enhanced Cheat Sheet ---
(defun my/show-cheatsheet ()
  (interactive)
  (with-output-to-temp-buffer "*Keybindings*"
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
    (princ "  M-t / M-w ...... New / Close tab\n\n")
    (princ "📁 Projects & Git:\n")
    (princ "  C-c p .......... Projectile prefix (enhanced)\n")
    (princ "  C-c p f ........ Find file in project\n")
    (princ "  C-c p s g ...... Grep in project\n")
    (princ "  C-x g .......... Magit status\n")
    (princ "  C-c g .......... Git grep (counsel)\n\n")
    (princ "🤖 LLM / ChatGPT:\n")
    (princ "  C-c C-g ........ Open GPTel chat\n")
    (princ "  C-c RET ........ Send prompt (inside chat buffer)\n\n")
    (princ "🔍 Context & Search:\n")
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
    (princ "  - Enhanced project management\n")))
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

;; --- Suppress end-of-file warnings ---
(setq warning-suppress-types '((initialization)))

(message "🚀 Enhanced Emacs IDE ready! Press C-k for keybindings cheat sheet.")
