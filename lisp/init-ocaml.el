;;; init-ocaml.el --- Defines all the ocaml preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'exec-path "/home/martin/.opam/miking-ocaml/bin")
(setenv "PATH"
        (concat "/home/martin/.opam/miking-ocaml/bin:"
                (getenv "PATH")))

(use-package tuareg
  :mode ("\\.ml[iylp]?\\'" . tuareg-mode)
  :hook (tuareg-mode . my/ocaml-setup)
  :config
  (setq tuareg-indent-align-with-first-arg t)

  (defun my/ocaml-setup ()
    (eglot-ensure)

    ;; Format only when eglot is active
    (add-hook 'before-save-hook
              #'my/eglot-format
              nil t))

  (defun my/eglot-format ()
    (when (eglot-current-server)
      (eglot-format-buffer))))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
