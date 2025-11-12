;;; init-org-mode.el --- Minimal Org-mode setup for TODO lists  -*- lexical-binding: t; -*-

;;; Commentary:
;; Force Org buffers to keep global (and minor-mode) keybindings only.

;;; Code:

;; At the top of init-org-mode.el, before calling it:
(autoload 'martin/text-setup "init-text" nil t)

(use-package org
  :ensure t
  :config

  (setq org-agenda-files '("~/kth/protocols/protocols.org"
                           "~/kth/security/security.org"
                           "~/kth/integrating/integrating.org"
                           "~/kth/philosophy/philosophy.org"
                           "~/Travail/miking-docgen/docgen.org"
                           "~/Travail/Dake/dake.org"
                           "~/.emacs.d/emacs.org"
                           "~/org/kth.org"
                           "~/org/kth_deadlines.org"
                           "~/org/notes.org"
                          ))

  ;; Basic Org setup

  (martin/text-setup)
  (flyspell-mode 1)

  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-todo-keywords '((sequence "TODO" "DONE")))
  (setq org-startup-folded t)
  (setq org-log-done 'time))


(provide 'init-org-mode)
