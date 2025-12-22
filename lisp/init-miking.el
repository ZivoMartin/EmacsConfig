;;; init-miking.el --- Defines all the miking preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

;; (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))

(defvar miking-mode-path (expand-file-name "miking-emacs" manual-installation-directory))
(add-to-list 'load-path miking-mode-path)

(require 'mcore-mode)
(require 'miking-syn-mode)

;; (add-to-list 'auto-mode-alist '("\\.mc\\'" . mcore-ts-mode))

(provide 'init-miking)
;;; init-miking.el ends here
