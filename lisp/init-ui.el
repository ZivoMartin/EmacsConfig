(require 'init-elpa)
(require-package 'atom-one-dark-theme)
(require-package 'golden-ratio)



(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-face-attribute 'default nil :height 140)
(setq-default line-spacing 0.4)

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(load-theme 'tango-dark)

(blink-cursor-mode 0)
(set-cursor-color "#cccccc")
(setq ring-bell-function 'ignore)

(provide 'init-ui)
