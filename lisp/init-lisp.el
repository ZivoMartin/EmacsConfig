;;; Configuration pour Lisp (Emacs Lisp, Common Lisp, Scheme)

;;; Support pour Emacs Lisp (builtin)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (display-line-numbers-mode) ;; Numéros de ligne
            (show-paren-mode)           ;; Met en surbrillance les parenthèses
            (eldoc-mode)))              ;; Affiche la documentation inline

;;; Activation de `paredit` pour une manipulation facile des S-expressions
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))

;;; Complétion intelligente avec `company`
(use-package company
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2))


;;; Support pour Scheme avec Geiser
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(guile racket chicken))
  (add-hook 'scheme-mode-hook 'geiser-mode))

;;; Affichage des parenthèses arc-en-ciel (utile pour Lisp)
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . rainbow-delimiters-mode))

;;; Formatage automatique du code avant la sauvegarde
(defun lisp-auto-format ()
  "Auto-indente le code avant la sauvegarde."
  (when (or (eq major-mode 'emacs-lisp-mode)
            (eq major-mode 'lisp-mode)
            (eq major-mode 'scheme-mode))
    (indent-region (point-min) (point-max))))

(add-hook 'before-save-hook 'lisp-auto-format)

(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(provide 'init-lisp)
