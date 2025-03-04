
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


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


(use-package eldoc
  :ensure t
  :custom (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))


(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package rainbow-mode
  :ensure t)
(add-hook 'prog-mode-hook 'rainbow-mode)

(use-package markdown-mode
  :ensure t)


(provide 'init-package-import)
