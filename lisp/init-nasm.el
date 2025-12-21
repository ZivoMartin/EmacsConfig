;;; init-nasm.el --- Defines all the nasm preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(use-package nasm-mode
  :hook (nasm-mode . eglot-ensure))



(provide 'init-nasm)
;;; init-nasm.el ends here
