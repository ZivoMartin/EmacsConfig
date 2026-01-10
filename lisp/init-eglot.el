;;; init-eglot.el --- Install eglot -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'init-use-package)

(use-package eglot
  :defer t
  :commands eglot-ensure)

(provide 'init-eglot)
;;; init-eglot.el ends here
