;; Activer rust-ts-mode (Tree-Sitter)
(use-package rust-ts-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-ts-mode) ;; Associe automatiquement rust-ts-mode aux fichiers .rs
  :hook ((rust-ts-mode . eglot-ensure)   ;; Active LSP avec Eglot
         (rust-ts-mode . company-tng-mode) ;; Complétion avec Company TNG
         (rust-ts-mode . (lambda ()
                           (eglot-inlay-hints-mode -1)))) ;; Désactive les hints inline
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (setq rust-format-on-save t)) ;; Formatage automatique avec rustfmt

;; Fonction pour ajouter les imports automatiquement
(defun rust-analyzer-add-missing-imports ()
  "Ajoute les imports manquants avec Rust Analyzer."
  (interactive)
  (eglot-code-actions nil nil "Add missing imports" t))

;; Associer la touche C-c C-i à l'ajout d'import
(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-i") 'rust-analyzer-add-missing-imports))

;; Compilation rapide avec C-c C-c
(add-hook 'rust-ts-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'rust-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-<return>") #'xref-find-definitions)) ;; C-Enter
(global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions)

(provide 'init-rust)
