;;; init-javascript.el --- JavaScript setup with eglot + ESLint -*- lexical-binding: t; -*-

;; Ensure eglot runs for JS/TS
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;; Enable flymake for diagnostics
(add-hook 'eglot-managed-mode-hook #'flymake-mode)

;; Auto-format on save
(defun my/eglot-format-on-save ()
  "Format buffer before saving when eglot is active."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'eglot-managed-mode-hook #'my/eglot-format-on-save)

;; Tell eglot to use both tsserver and eslint
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode)
                 . ("vscode-eslint-language-server" "--stdio"))))

;; Keybindings: jump to definitions (M-.), back (M-,)
;; Already provided by xref

(provide 'init-javascript)
;;; init-javascript.el ends here
