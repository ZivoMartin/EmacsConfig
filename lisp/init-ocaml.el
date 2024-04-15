(require 'init-elpa)
(use-package tuareg
  :ensure t
  :hook (tuareg-mode . lsp-deferred))

(add-to-list 'exec-path "~/.opam/default/bin")

(use-package ocamlformat
  :ensure t
  :bind ("<f6>" . ocamlformat))
(provide 'init-ocaml)
