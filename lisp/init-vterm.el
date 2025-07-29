;;; init-vterm.el --- VTerm configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for `vterm`, including custom helpers:
;; - Split terminal to the right (`C-c t`)
;; - Open a new vterm buffer (`C-c n`)

;;; Code:

(require 'init-elpa)

(use-package vterm
  :ensure t
  :demand t
  :commands (vterm)
  :config
  ;; Open a new vterm in a right-side split
  (defun my/vterm-split-right ()
    "Split the window to the right and open a new vterm."
    (interactive)
    (when (split-window-right)
      (other-window 1)
      (vterm)))

  ;; Open a uniquely named vterm buffer
  (defun my/vterm-new-buffer ()
    "Open a new vterm buffer with a unique name."
    (interactive)
    (let ((buffer (generate-new-buffer-name "vterm")))
      (vterm buffer)))

  ;; Keybindings
  (global-set-key (kbd "C-c t") #'my/vterm-split-right)
  (global-set-key (kbd "C-c n") #'my/vterm-new-buffer))

(provide 'init-vterm)
;;; init-vterm.el ends here
