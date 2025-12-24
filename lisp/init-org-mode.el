;;; init-org-mode.el --- Minimal Org-mode setup for TODO lists  -*- lexical-binding: t; -*-

;;; Commentary:
;; Force Org buffers to keep global (and minor-mode) keybindings only.

;;; Code:

;; At the top of init-org-mode.el, before calling it:
(autoload 'martin/text-setup "init-text" nil t)

(use-package org
  :ensure t
  :config

  (setq org-agenda-files '("~/Projects/Fibo/fibo.org"
                           "~/Projects/miking-docgen/docgen.org"
                           "~/.emacs.d/emacs.org"
                           "~/kth/kth.org"
                           "~/Random/random.org"
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
