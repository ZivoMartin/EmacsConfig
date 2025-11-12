;;; init-dashboard.el --- Dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Clean, modern Emacs Dashboard setup with banner, icons, items,
;; navigation buttons, and custom headings.

;;; Code:

(use-package dashboard
  :ensure t
  :defer t
  :hook (after-init . dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  (dashboard-set-navigator nil)
  (dashboard-set-init-info t)
  (dashboard-banner-logo-title "Heee game is game")
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents   . 10)
                     (projects  . 5)
                     (agenda    . 5)
                     (bookmarks . 5)
                     (registers . 3))))


(provide 'init-dashboard)
;;; init-dashboard.el ends here
