;;; init-org.el --- Defines all the org preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :config
  (setq org-agenda-files '("~/Projects/miking-docgen/docgen.org"
                           "~/Projects/Dake/dake.org"
                           "~/.emacs.d/emacs.org"
                           "~/kth/kth.org"
                           "~/Random/random.org")
        )
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

(provide 'init-org)
;;; init-org.el ends here
