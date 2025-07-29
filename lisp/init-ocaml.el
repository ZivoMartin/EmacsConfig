;;; init-ocaml.el --- OCaml language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; OCaml programming environment using `tuareg-mode` and `eglot` with `ocaml-lsp-server`.

;;; Code:

(use-package tuareg
  :ensure t
  :mode ("\\.ml[iylp]?\\'" . tuareg-mode)
  :init
  (setq tuareg-mode-map (make-sparse-keymap))  ;; Nuke all keybindings
  :hook (tuareg-mode . my/ocaml-setup))

(defun my/ocaml-setup ()
  "Setup OCaml development environment with LSP and formatting."
  (eglot-ensure)                             ;; Start LSP
  (setq indent-tabs-mode nil)
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-along-line t))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
