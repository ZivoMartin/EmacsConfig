(require 'init-elpa)

;; Configuration pour Tuareg et lsp-mode
(use-package tuareg
  :ensure t
  :hook (tuareg-mode . lsp-deferred))

;; Ajouter le chemin OPAM à exec-path
(add-to-list 'exec-path "~/.opam/default/bin")

(use-package lsp-mode
  :ensure t
  :commands lsp-deferred
  :hook ((tuareg-mode . lsp-deferred)
         (reason-mode . lsp-deferred))
  :config
  (setq lsp-prefer-flymake nil)) 


(use-package lsp-ocaml
  :after lsp-mode
  :ensure nil ;; lsp-ocaml est intégré à lsp-mode, pas besoin d'installation séparée
  :custom
  (lsp-ocaml-server-command '("ocamllsp")))

;; Ajouter ocamlformat pour le formatage automatique
(use-package ocamlformat
  :ensure t
  :hook (before-save . ocamlformat-before-save)
  :config
  (setq ocamlformat-enable 'enable-on-save)) ;; Active l'autoformatage lors de la sauvegarde

;; Utiliser flycheck pour le soulignage des erreurs
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(provide 'init-ocaml)
