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
  (setq mc/always-repeat-command t)
  (setq mc/always-run-for-all t)
  (setq mc/list-file (expand-file-name "mc-lists.el" user-emacs-directory))

(provide 'init-multi-cursor)
;;; init-multi-cursor.el ends here
