;;; init-backups.el --- Defines all the BACKUPS preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq backup-directory-alist '((".*" . "~/.emacs-backups")))
(setq make-backup-files t) ;; Is actually already true, but you never know ;)

(setq auto-save-file-name-transforms `((".*" "~/.emacs-autosaves/" t)))
(setq auto-save-list-file-prefix "~/.emacs-autosaves/sessions-")

(provide 'init-backups)
;;; init-backups.el ends here
