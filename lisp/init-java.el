;;; init-java.el --- Simple Java setup with eglot -*- lexical-binding: t; -*-

(setenv "JAVA_HOME" "/usr/lib/jvm/java-21-openjdk")

;; Basic Java setup using eglot
(use-package eglot
  :ensure t
  :hook
  (java-mode . eglot-ensure)
  :config
  ;; Auto-format buffer before saving
  (add-hook 'java-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

;; Optional: better error highlighting through flymake (enabled by default)
(use-package flymake
  :hook (java-mode . flymake-mode))

(provide 'init-java)
;;; init-java.el ends here
