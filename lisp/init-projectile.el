;;; init-projectile.el --- Fast Projectile setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Lean config for Projectile
;; - Projectile: caching + alien indexing for speed.

;;; Code:

;; ----------------------
;; Projectile
;; ----------------------
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Travail" "~/.emacs.d" "~/kth"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien) ;; use git/rg, faster
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-indexing-method 'native))

(provide 'init-projectile)
;;; init-projectile.el ends here
