;;; init-haskell.el --- Haskell configuration using Eglot

;;; Commentary:
;; Provides syntax highlighting, indentation, LSP support via HLS,
;; and formatting on save.

;;; Code:

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook ((haskell-mode . eglot-ensure)
         (haskell-mode . my/haskell-format-on-save))
  :config
  ;; Indentation defaults
  (setq haskell-indentation-layout-offset 4
        haskell-indentation-starter-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2))

;; Formatting with stylish-haskell or ormolu
(defun my/haskell-format-on-save ()
  "Format Haskell code on save using stylish-haskell or ormolu."
  (when (executable-find "stylish-haskell")
    (add-hook 'before-save-hook #'haskell-mode-stylish-buffer nil t)))

(provide 'init-haskell)
;;; init-haskell.el ends here
