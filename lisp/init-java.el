;;; init-java.el --- Defines all the java preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq lsp-log-io t)
(setq lsp-log-max 10000)

(use-package lsp-java
  :hook (java-mode . lsp)
  :config
  (setq lsp-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java"))

(provide 'init-java)
;;; init-java.el ends here
