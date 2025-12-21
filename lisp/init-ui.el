;;; init-ui.el --- Defines all the UI preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

(blink-cursor-mode 0)
(set-cursor-color "#ccccad")

(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

(setq-default line-spacing 0.4)
(setq-default apropos-do-all t)

(set-face-attribute 'default nil :height 180)

(setq split-height-threshold 160)
(setq split-width-threshold 100)

(global-display-line-numbers-mode 1)
(column-number-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-ui)
;;; init-ui.el ends here
