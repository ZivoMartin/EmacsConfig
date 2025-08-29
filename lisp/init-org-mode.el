;;; init-org-mode.el --- Minimal Org-mode setup for TODO lists  -*- lexical-binding: t; -*-

;;; Commentary:
;; Force Org buffers to keep global (and minor-mode) keybindings only.

;;; Code:

;; At the top of init-org-mode.el, before calling it:
(autoload 'martin-global-mode "init-bindings" nil t)

(use-package org
  :ensure t
  :config

  (setq org-agenda-files '("~/kth/protocols/protocols.org"
                           "~/kth/security/security.org"
                           "~/kth/integrating/integrating.org"
                           "~/Travail/miking-docgen/todo.org"
                           "~/.emacs.d/emacs.org"
                           "~/org/kth.org"
                           "~/org/notes.org"
                          ))

  ;; Basic Org setup
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-todo-keywords '((sequence "TODO" "DONE")))
  (setq org-startup-folded t)
  (setq org-log-done 'time))

(declare-function martin-global-mode "init-bindings" (&optional arg))

(with-eval-after-load 'org
  (martin-global-mode 1))

(provide 'init-org-mode)
;;; init-org-mode.el ends here
