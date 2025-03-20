;;; init-java.el --- Java configuration for Emacs

;;; Commentary:
;; This configuration enables LSP support, syntax checking, autocompletion,
;; and debugging support for Java development in Emacs.

;;; Code:

(require 'package)

;; Initialize package sources
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Ensure `use-package` is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Java LSP support
(use-package lsp-mode
  :ensure t
  :hook (java-mode . lsp)
  :commands lsp
  :config
  (setq lsp-java-java-path "java"
        lsp-java-save-action-organize-imports t
        lsp-java-format-on-type-enabled t))

(use-package lsp-java
  :ensure t
  :after lsp-mode)

;; Autocompletion with Company
(use-package company
  :ensure t
  :hook (java-mode . company-mode))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :hook (java-mode . flycheck-mode))

;; Debug Adapter Protocol (DAP) for Java
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-java))

;; Disable electric indent mode in Java
(defun my-java-disable-electric-indent ()
  "Disable electric indent mode in Java."
  (electric-indent-local-mode -1))
(add-hook 'java-mode-hook #'my-java-disable-electric-indent)

;; Fix text block handling in Java
(defun my-java-fix-text-blocks ()
  "Ensure correct syntax highlighting for Java text blocks."
  (modify-syntax-entry ?\" "\"" java-mode-syntax-table))

(add-hook 'java-mode-hook #'my-java-fix-text-blocks)

;; Tree-sitter support for Java
(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package tree-sitter
  :hook (java-mode . tree-sitter-hl-mode))

(provide 'init-java)
;;; init-java.el ends here
