;;; pf-mode.el --- Major mode for pf runner files  -*- lexical-binding: t; -*-

(define-generic-mode 'pf-mode
  '(?#) ;; Comment start
  '("task" "end" "describe" "shell" "packages" "directory" "copy" "env" "service" "include") ;; Keywords
  '(("^\\s-*task\\s-+\\([a-zA-Z0-9_-]+\\)" 1 font-lock-function-name-face)
    ("^\\s-*describe\\s-+\\(.*\\)" 1 font-lock-doc-face)
    ("\\b\\(install\\|remove\\|start\\|stop\\|enable\\|disable\\|restart\\)\\b" . font-lock-constant-face)
    ("\\$[a-zA-Z0-9_]+" . font-lock-variable-name-face)
    ("^#!.*" . font-lock-comment-face))
  '("\\.pf\\'") ;; Auto-mode-alist
  nil ;; Function list
  "Major mode for pf runner files.")

(provide 'pf-mode)
;;; pf-mode.el ends here
