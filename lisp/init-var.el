;; Define a central "var" directory inside user-emacs-directory
(defvar my-emacs-var-dir (expand-file-name "var/" user-emacs-directory))

(setq
 ;; Backups
 backup-directory-alist `(("." . ,(expand-file-name "backups/" my-emacs-var-dir)))
 auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" my-emacs-var-dir)
 auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" my-emacs-var-dir) t))

 ;; Custom file
 custom-file (expand-file-name "custom.el" my-emacs-var-dir)

 ;; Recent files
 recentf-save-file (expand-file-name "recentf" my-emacs-var-dir)

 ;; TRAMP
 tramp-persistency-file-name (expand-file-name "tramp" my-emacs-var-dir)

 ;; ELPA packages → stored in var/elpa
 package-user-dir (expand-file-name "elpa/" my-emacs-var-dir)
 package-directory-list (list "/usr/share/emacs/site-lisp/elpa")

 ;; Places, history, projects
 save-place-file (expand-file-name "places" my-emacs-var-dir)
 savehist-file (expand-file-name "history" my-emacs-var-dir)
 projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-emacs-var-dir)
 mc/list-file (expand-file-name "mc-lists.el" my-emacs-var-dir)

 ;; URL and transient state
 url-history-file (expand-file-name "url/history" my-emacs-var-dir)
 transient-history-file (expand-file-name "transient/history.el" my-emacs-var-dir)
 transient-levels-file (expand-file-name "transient/levels.el" my-emacs-var-dir)
 transient-values-file (expand-file-name "transient/values.el" my-emacs-var-dir))

(provide 'init-var)
