;;; init-c.el --- C/C++ configuration using Eglot

;;; Commentary:
;; Provides lightweight IDE features for C/C++ using eglot + clangd.
;; Includes go-to-definition, completion, diagnostics, formatting.

;;; Code:

;; Major modes
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.h\\'" . c++-mode))
  :config
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil))

;; Eglot is built-in (since Emacs 29), or install via ELPA for Emacs 28
(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

;; Formatting with clang-format on save
(use-package clang-format
  :hook ((c-mode . my/clang-format-on-save)
         (c++-mode . my/clang-format-on-save)))

(defun my/clang-format-on-save ()
  "Format C/C++ buffer on save with clang format."
  (add-hook 'before-save-hook #'clang-format-buffer nil t))

(provide 'init-c)
;;; init-c.el ends here


