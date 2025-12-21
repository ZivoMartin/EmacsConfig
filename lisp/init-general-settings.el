;;; init-general-settings.el --- Defines all the general settings, basically general variables. -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:


(defvar manual-installation-directory
  (expand-file-name "manual" user-emacs-directory)
  "Path to the folder containing the manual installations.")

(defvar source-code-directory
  (expand-file-name "lisp" user-emacs-directory)
  "Path to the folder containing the config source code.")

(setq enable-local-variables :safe)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(setq trusted-content
      (list (file-name-as-directory source-code-directory)))

(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)

(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)

(provide 'init-general-settings)
;;; init-general-settings.el ends here
