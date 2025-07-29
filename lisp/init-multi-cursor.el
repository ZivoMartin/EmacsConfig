;;; init-multi-cursor.el --- Multi-cursor support -*- lexical-binding: t; -*-

;; Keywords: editing, cursors
;; Package-Requires: ((emacs "26.1") (multiple-cursors "1.4.0"))

;;; Commentary:

;; This file sets up multiple-cursors with ergonomic keybindings and
;; optional region bindings mode for enhanced selection-based usage.

;;; Code:

;; Install with:
;; M-x package-install RET multiple-cursors RET

(use-package multiple-cursors
  :ensure t
  :commands
  (mc/edit-lines
   mc/mark-all-like-this
   mc/mark-next-like-this
   mc/mark-previous-like-this
   mc/mark-all-like-this-dwim
   mc/mark-more-like-this-extended)
  :init
  ;; Optional: avoid accidental cursor creep
  (setq mc/list-file (expand-file-name "mc-lists.el" user-emacs-directory))
  :bind
  (("C-c m l" . mc/edit-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m a" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m b" . mc/edit-beginnings-of-lines)
   ("C-c m r" . set-rectangular-region-anchor)
   ("C-c m d" . mc/mark-more-like-this-extended)))


;; Optional: enable region-based bindings for multiple cursors
(use-package region-bindings-mode
  :ensure t
  :after multiple-cursors
  :config
  (region-bindings-mode-enable)
  (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
  (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
  (define-key region-bindings-mode-map "d" 'mc/mark-all-like-this-dwim))

(provide 'init-multi-cursor)
;;; init-multi-cursor.el ends here
