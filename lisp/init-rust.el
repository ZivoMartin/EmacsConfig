(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp))

(use-package eglot
  :ensure t
  :demand t
  :bind (:map eglot-mode-map
	      ("<f6>" . eglot-format-buffer)
	      ("C-c a" . eglot-code-actions)
	      ("C-c d" . eldoc)
	      ("C-c r" . eglot-rename))

  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))


(use-package magit
  :ensure t)

(use-package company
  :ensure t
  :hook (emacs-lisp-mode . company-tng-mode)
  :config
  (setq company-idle-delay 0.5
	company-minimum-prefix-length 2))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map))


(use-package eldoc
  :ensure t
  :custom (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

(setq read-process-output-max (* 1024 1024))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)


(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . company-mode))
  :custom
  (lsp-rust-analyzer-server-command '("rust-analyzer"))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-diagnostics-enable-experimental t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-analyzer-proc-macro-enable t))


(use-package rust-mode
  :ensure t)

(add-to-list 'load-path "rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package rust-ts-mode
  :ensure t
  :after (eglot)
  :hook ((rust-ts-mode . eglot-ensure)
	 (rust-ts-mode . company-tng-mode)
	 (rust-ts-mode . (lambda ()
			   (eglot-inlay-hints-mode -1))))
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))
(add-hook 'rust-mode-hook #'lsp)


(defun rust-analyzer-add-missing-imports ()
  "Runs the 'rust-analyzer.add-missing-imports' code action."
  (interactive)
  (let ((action (seq-find
                 (lambda (action)
                   (string-equal (gethash "command" action) "rust-analyzer.add-missing-imports"))
                 (lsp-code-actions-at-point))))
    ))

(define-key rust-mode-map (kbd "C-c C-i") 'rust-analyzer-add-missing-imports)
(unless (package-installed-p 'flycheck)
  (package-refresh-contents)
  (package-install 'flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)
(put 'downcase-region 'disabled nil)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'compile)))
(provide 'init-rust)
