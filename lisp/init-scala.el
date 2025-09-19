;;; init-scala.el -- Configuration file for the Scala programming language -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :mode ("\\.scala\\'" . scala-mode)
  )

(use-package eglot
  :hook (scala-mode . eglot-ensure))


(provide 'init-scala)
;;; init-scala.el ends here
