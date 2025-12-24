;;; init-roc.el --- Minimal regex-based Roc syntax highlighting

(defvar roc-font-lock-keywords
  `((,(regexp-opt '("app" "platform" "import") 'words)
     . font-lock-keyword-face)
    ("[A-Z][A-Za-z0-9_]*" . font-lock-type-face)
    ("#.*$" . font-lock-comment-face))
  "Basic Roc syntax rules.")

(define-derived-mode roc-mode prog-mode "Roc"
  "Minimal Roc mode with regex syntax highlighting only."
  (setq font-lock-defaults '(roc-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.roc\\'" . roc-mode))

(provide 'init-roc)
;;; init-roc.el ends here
