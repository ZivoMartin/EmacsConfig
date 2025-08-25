;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Projectile setup with directory-by-directory completion for files.
;; Press TAB to complete only up to the next folder segment (e.g., "folder1/"),
;; instead of autocompleting to a full file.

;;; Code:

;; Ensure use-package is available
(eval-when-compile
  (require 'use-package))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  ;; Define the prefix key for projectile (used by projectile’s own maps)
  (setq projectile-keymap-prefix (kbd "C-c p"))

  ;; Make file prompts do *directory-wise* completion.
  ;; This affects any vanilla completing-read UI (Vertico/Default minibuffer).
  ;; - 'partial-completion makes "fol<TAB>" -> "folder1/"
  ;; - Keep others for flexibility; override only the *file* category.
  (setq completion-styles '(substring partial-completion flex))
  (setq completion-category-overrides '((file (styles partial-completion))))

  :config
  ;; Enable projectile globally
  (projectile-mode +1)

  ;; Indexing & caching
  (setq projectile-indexing-method 'alien)   ;; or 'native
  (setq projectile-enable-caching t)

  ;; Completion system selection.
  ;; We prefer *default* here so we get the file-category completion
  ;; behavior (partial-completion). If Helm is present, it already
  ;; supports hierarchical navigation nicely, so use it.
  (cond
   ((featurep 'helm)
    (setq projectile-completion-system 'helm))
   (t
    (setq projectile-completion-system 'default)))

  ;; If Ivy is installed, tell Ivy to *not* hijack Projectile’s
  ;; completing-read calls. This lets our file-category completion
  ;; (partial-completion) take effect for projectile prompts.
  (with-eval-after-load 'ivy
    ;; Fallback to the original completing-read for Projectile callers.
    ;; `nil` means “don’t use Ivy for this caller”.
    (add-to-list 'ivy-completing-read-handlers-alist
                 '(projectile-completing-read . nil))
    (add-to-list 'ivy-completing-read-handlers-alist
                 '(projectile-switch-project . nil))
    (add-to-list 'ivy-completing-read-handlers-alist
                 '(projectile-find-file . nil)))

  ;; Optional: if you *do* use Helm, this makes things even nicer.
  (with-eval-after-load 'helm
    (require 'helm-projectile nil t))

  ;; Search path for auto-discovery of projects
  (setq projectile-project-search-path '("~"))

  ;; Action on switch-project
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'init-projectile)
;;; init-projectile.el ends here
