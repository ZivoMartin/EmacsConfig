;;; init-drag-stuff.el --- Drag Stuff configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures drag-stuff mode so you can move lines or regions
;; with M-up / M-down / M-left / M-right.

;;; Code:


(use-package drag-stuff
  :ensure t
  :hook (after-init . drag-stuff-global-mode)
  :bind (("M-<up>"    . drag-stuff-up)
         ("M-<down>"  . drag-stuff-down)
         ("M-<left>"  . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)))

(provide 'init-drag-stuff)
;;; init-drag-stuff.el ends here
