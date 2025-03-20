;; Haskell Mode
(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . lsp))
  :config
  (setq haskell-process-type 'auto))  ;; Détecte GHCi, Stack ou Cabal automatiquement

;; LSP pour Haskell
(use-package lsp-haskell
  :ensure t
  :after lsp-mode
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"))

;; Company pour la complétion automatique
(use-package company
  :ensure t
  :hook (haskell-mode . company-mode))

;; Flycheck pour les erreurs
(use-package flycheck
  :ensure t
  :hook (haskell-mode . flycheck-mode))

;; YASnippet pour les snippets de code
(use-package yasnippet
  :ensure t
  :hook (haskell-mode . yas-minor-mode))

;; Mode d'affichage structuré pour Haskell
(use-package hindent
  :ensure t
  :hook (haskell-mode . hindent-mode))



(provide 'init-haskell)
