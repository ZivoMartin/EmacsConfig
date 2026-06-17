;;; init-backups.el --- Backup & autosave config -*- lexical-binding: t; -*-

;; Ensure directories exist
(make-directory "~/.emacs-backups/" t)
(make-directory "~/.emacs-autosaves/" t)

;; Backup files (file~)
(setq backup-directory-alist
      '((".*" . "~/.emacs-backups/")))
(setq make-backup-files t)

;; Auto-save files (#file#)
(setq auto-save-default t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs-autosaves/" t)))

;; Auto-save session files
(setq auto-save-list-file-prefix
      "~/.emacs-autosaves/sessions-")

(provide 'init-backups);;; init-backups.el --- Backup & autosave config -*- lexical-binding: t; -*-

;; Ensure directories exist
(make-directory "~/.emacs-backups/" t)
(make-directory "~/.emacs-autosaves/" t)

;; Backup files (file~)
(setq backup-directory-alist
      '((".*" . "~/.emacs-backups/")))
(setq make-backup-files t)

;; Auto-save files (#file#)
(setq auto-save-default t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs-autosaves/" t)))

;; Auto-save session files
(setq auto-save-list-file-prefix
      "~/.emacs-autosaves/sessions-")

(provide 'init-backups)
