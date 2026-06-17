;;; init-scala.el --- Defines all the scala preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode)
  :hook (scala-mode . eglot-ensure)
  :interpreter
    ("scala" . scala-mode))


(provide 'init-scala)
;;; init-scala.el ends here
