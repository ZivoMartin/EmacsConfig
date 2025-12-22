;;; init-c.el --- Defines all the c preferences -*- lexical-bindings: t; -*-

;;; Commentary:
;;; Command to generate a .clang-format file: clang-format -style=llvm -dump-config > .clang-format

;;; Code:

(use-package clang-format
  :defer t
  :commands (clang-format-buffer)
  :custom
  (clang-format-style "Google"))

(defun martin-clang-format-on-save ()
  "Format C/C++ buffer on save with clang format."
  (add-hook 'before-save-hook #'clang-format-buffer nil t))

(use-package cc-mode
  :after clang-format
  :mode (("\\.cpp\\'" . c++-mode)
        ("\\.c\\'" . c-mode)
        ("\\.hpp\\'" . c++-mode)
        ("\\.h\\'" . c-mode))
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode . clang-format-on-save-mode)
         (c++-mode . clang-format-on-save-mode)
         (c-mode . martin-clang-format-on-save)
         (c++-mode . martin-clang-format-on-save)))


(provide 'init-c)
;;; init-c.el ends here
