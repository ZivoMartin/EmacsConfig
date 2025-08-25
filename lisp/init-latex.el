;;; init-latex.el --- Minimal LaTeX configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file sets up a minimalist LaTeX environment in Emacs using AUCTeX.
;; It includes basic syntax highlighting, PDF compilation, previewing, and
;; sensible defaults for editing LaTeX documents.

;;; Code:

;; Ensure AUCTeX is installed
(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . TeX-source-correlate-mode))
  :config
  ;; Use PDF by default
  (setq TeX-PDF-mode t)

  ;; Automatically save before compiling
  (setq TeX-save-query nil)

  ;; Automatically parse the document to enable features like autocompletion
  (setq TeX-auto-save t
        TeX-parse-self t)

  ;; Enable synctex correlation (source <-> PDF)
  (setq TeX-source-correlate-start-server t)

  ;; Viewer setup (adjust as needed)
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools"))
        TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Enable forward/inverse search with PDF Tools
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; RefTeX for managing labels, citations, etc.
(use-package reftex
  :defer t
  :config
  (setq reftex-plug-into-AUCTeX t))


(provide 'init-latex)
;;; init-latex.el ends here
