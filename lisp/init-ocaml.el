;;; init-ocaml.el --- Defines all the ocaml preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(use-package tuareg
  :mode ("\\.ml[iylp]?\\'" . tuareg-mode)
  :hook (tuareg-mode . eglot-ensure)
  :config
  (setq tuareg-indent-align-with-first-arg t))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
