;;; init-ui.el --- UI customization settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file customizes the appearance and interface behavior:
;; - Disables GUI elements (menu, toolbar, scrollbars)
;; - Sets fonts, line spacing, cursor, theme
;; - Enables visual helpers like rainbow delimiters and golden-ratio
;; - Activates line numbers in programming buffers

;;; Code:

;;; Disable GUI elements
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; Startup appearance
(setq inhibit-startup-message t)
(blink-cursor-mode 0)
(set-cursor-color "#cccccc")

;;; Font and spacing
(set-face-attribute 'default nil :height 120)
(setq-default line-spacing 0.4)

;;; Clipboard & selection behavior
(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

;;; Disable bell sound
(setq ring-bell-function 'ignore)

(setq use-dialog-box nil)

;;; Theme configuration
(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

;;; Dynamic window resizing with golden ratio
(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

;;; Highlight nested parentheses by color in code
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Show line numbers in programming buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(provide 'init-ui)
;;; init-ui.el ends here
