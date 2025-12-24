;;; init-projectile.el --- Defines all the projectile preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package projectile
  :demand t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/Projects" "~/.emacs-save.d/" "Archives" "Random" "~/.emacs.d" "~/kth"))
  (setq projectile-globally-ignored-directories '("node_modules" "target" "dist" ".cache" ".git"))
  (setq projectile-enable-caching t)
  (projectile-discover-projects-in-search-path)
  (setq projectile-indexing-method 'alien))

(provide 'init-projectile)
;;; init-projectile.el ends here
