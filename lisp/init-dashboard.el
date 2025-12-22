;;; init-dashboard.el --- Defines all the dashboard preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-banner-logo-title "How is it going gang")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  (setq dashboard-navigation-cycle t))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
