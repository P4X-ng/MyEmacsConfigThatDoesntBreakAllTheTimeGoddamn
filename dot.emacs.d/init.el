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

;; --- VSCode-like Theming ---
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; --- Modern Modeline (VSCode-like status bar) ---
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil))

;; --- All-the-icons (for modeline and treemacs) ---
(use-package all-the-icons
  :if (display-graphic-p))

;; --- Which-key (Command palette hints like VSCode) ---
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom))

;; --- Multiple cursors (like VSCode) ---
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; --- Avy (Quick navigation like VSCode's Go to Symbol) ---
(use-package avy
  :bind (("C-'" . avy-goto-char-2)
         ("C-;" . avy-goto-line)))

;; --- Smartparens (Bracket pairing like VSCode) ---
(use-package smartparens
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-paren-mode t))

;; --- Rainbow delimiters (Colorful brackets) ---
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- Highlight indent guides (like VSCode) ---
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))

;; --- Better undo system (like VSCode's undo) ---
(use-package undo-tree
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; --- Tab-bar mode (VSCode-like tabs) ---
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)
(global-set-key (kbd "M-<left>") 'tab-previous)
(global-set-key (kbd "M-<right>") 'tab-next)
(global-set-key (kbd "M-t") 'tab-new)
(global-set-key (kbd "M-w") 'tab-close)

;; --- VSCode-like keybindings ---
(global-set-key (kbd "C-/") 'comment-line)  ; Comment/uncomment line
(global-set-key (kbd "C-d") 'kill-whole-line)  ; Delete line
(global-set-key (kbd "C-S-k") 'kill-whole-line)  ; Alternative delete line
(global-set-key (kbd "M-<up>") (lambda () (interactive) (transpose-lines 1) (forward-line -2)))  ; Move line up
(global-set-key (kbd "M-<down>") (lambda () (interactive) (forward-line 1) (transpose-lines 1) (forward-line -1)))  ; Move line down
(global-set-key (kbd "C-p") 'find-file)  ; Quick open (like Ctrl+P in VSCode)
(global-set-key (kbd "C-S-f") 'rgrep)  ; Search in files
(global-set-key (kbd "C-b") 'switch-to-buffer)  ; Quick switch buffer

;; --- Completion + Orderless (VSCode IntelliSense-like) ---
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize t))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

(use-package corfu 
  :init (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-scroll-margin 5))

(use-package orderless
  :after corfu
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --- Cape (Completion backends for better suggestions) ---
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

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

;; --- Git (No projects) ---
(use-package magit :commands magit-status :bind ("C-x g" . magit-status))

;; --- Treemacs (File explorer like VSCode) ---
(use-package treemacs
  :defer t
  :bind (("<f8>" . treemacs))
  :config 
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-fringe-indicator-mode 'always-visible))

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

;; --- Clean treemacs cache ---
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
    (princ "Emacs VSCode-like IDE Keybindings\n================================\n\n")
    (princ "File & Navigation:\n")
    (princ "  F8 ............. Toggle Treemacs sidebar (file explorer)\n")
    (princ "  C-p ............ Quick open file (like Ctrl+P in VSCode)\n")
    (princ "  C-b ............ Switch buffer (like Ctrl+Tab)\n")
    (princ "  C-S-f .......... Search in files (like Ctrl+Shift+F)\n\n")
    (princ "Tabs:\n")
    (princ "  M-← / M-→ ...... Switch tabs\n")
    (princ "  M-t / M-w ...... New / Close tab\n\n")
    (princ "Editing:\n")
    (princ "  C-/ ............ Comment/uncomment line\n")
    (princ "  C-d / C-S-k .... Delete line\n")
    (princ "  M-↑ / M-↓ ...... Move line up/down\n")
    (princ "  C-> / C-< ...... Mark next/previous like this (multi-cursor)\n")
    (princ "  C-S-c C-S-c .... Edit multiple lines\n\n")
    (princ "Navigation:\n")
    (princ "  C-' ............ Jump to character (Avy)\n")
    (princ "  C-; ............ Jump to line (Avy)\n\n")
    (princ "Git:\n")
    (princ "  C-x g .......... Magit status\n\n")
    (princ "LLM / ChatGPT:\n")
    (princ "  C-c C-g ........ Open GPTel chat\n")
    (princ "  C-c RET ........ Send prompt (inside chat buffer)\n\n")
    (princ "Context Helpers:\n")
    (princ "  C-c s .......... Search & insert from context dirs\n")
    (princ "  C-c r a ........ Add context dir\n")
    (princ "  C-c r r ........ Remove context dir\n\n")
    (princ "Python venvs:\n")
    (princ "  C-c v a ........ Activate\n")
    (princ "  C-c v d ........ Deactivate\n")
    (princ "  C-c v s ........ Show active venv\n\n")
    (princ "LSP:\n")
    (princ "  C-c l .......... LSP command prefix\n\n")
    (princ "Help:\n")
    (princ "  C-k ............ Show this cheat sheet\n")
    (princ "  C-h k .......... Describe key\n")
    (princ "  C-h f .......... Describe function\n")))
(global-set-key (kbd "C-k") #'my/show-cheatsheet)

;; --- Suppress end-of-file warnings ---
(setq warning-suppress-types '((initialization)))

(message "✅ VSCode-like Emacs IDE ready! Press C-k for keybindings.")
