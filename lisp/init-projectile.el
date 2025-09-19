;;; init-projectile.el --- Fast Projectile + Helm setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Lean config for Projectile + Helm.
;; - Projectile: caching + alien indexing for speed.
;; - Helm: slimmed sources, fast find-files.
;; - Helm-Projectile: integrates everything.
;; - Extra: GC optimization in minibuffer to avoid freezes.

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

;; ----------------------
;; Helm
;; ----------------------
(use-package helm
  :ensure t
  :init
  ;; GC optimization in minibuffer (avoids lag)
  (defun my/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my/minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))
  (add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

  ;; Slim down sources to avoid slow lookups
  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-projectile-files-list))

  :config
  (helm-mode 1)

  ;; Navigation
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-j") #'helm-previous-line)
    (define-key helm-map (kbd "C-k") #'helm-next-line)
    (define-key helm-map (kbd "C-p") #'delete-backward-char)))

;; ----------------------
;; Helm-Projectile
;; ----------------------
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
