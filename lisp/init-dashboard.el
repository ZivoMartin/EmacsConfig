;;; init-dashboard.el --- Dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, modern Emacs Dashboard setup with a custom banner, icons,
;; quick navigation, and personalized footer.

;;; Code:

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)

  :custom
  ;; General layout
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)

  ;; Banner setup
  ;; Options: 'official, 'logo, 'nil, or a string path to a custom image
  (dashboard-banner-logo-title "Heee game is game")
  (dashboard-startup-banner 'logo)

  ;; Items to display (you can reorder or remove any)
  (dashboard-items '((recents   . 10)
                     (projects  . 5)
                     (agenda    . 5)
                     (bookmarks . 5)
                     (registers . 3)))

  ;; Quick navigation buttons (optional)
  (dashboard-navigator-buttons
   `(((,"Open Config"
       "Edit your Emacs configuration"
       "M-x find-file ~/.emacs.d/init.el"
       (lambda (&rest _) (find-file "~/.emacs.d/init.el")))
      (,"Projects"
       "Open a project via Projectile"
       "C-c p p"
       (lambda (&rest _) (call-interactively 'projectile-switch-project)))
      (,"Hacker News"
       "Read the top stories"
       ""
       (lambda (&rest _) (dashboard-hackernews-insert)))))))

;; Optional visual tweak: nicer headings
(with-eval-after-load 'dashboard
  (setq dashboard-item-names '(("Recent Files:" . "Recent Files:")
                               ("Projects:"     . "Projects:")
                               ("Agenda for today:" . "Agenda:")
                               ("Bookmarks:"   . "Bookmarks:"))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
