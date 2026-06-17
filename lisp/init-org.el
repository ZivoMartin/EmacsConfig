;;; init-org.el --- Defines all the org preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :config
  (setq org-agenda-files '("~/agenda.org"
                           "~/Projects/disco/disco.org"
                           "~/Projects/disco/Explorer/explorer.org"
                           "~/Projects/disco/Resolver/resolver.org"
                           "~/Projects/disco/Vider/vider.org"
                           "~/Projects/disco/Deployer/deployer.org"
                           ;; "~/Projects/NotifierHub/notifier.org"
                           "~/.emacs.d/emacs.org"
                           "~/kth/agenda.org"
                           "~/kth/kth.org"))
  (setq org-todo-keywords '((sequence "TODO" "DONE")))
  (setq org-startup-folded 'fold)
  (setq org-log-done nil))

(defun martin-org-todo ()
  "Call `org-todo` if the major mode is `org-mode`."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-todo)))

(defun martin-org-insert-heading ()
  "Call `org-insert-heading` if the major mode is `org-mode`."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-insert-heading)))

(defun martin-org-insert-date ()
  "Insert the begining of a date for an event."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (insert "SCHEDULED: <2026-0>")
    (forward-char -1)))


(provide 'init-org)
;;; init-org.el ends here
