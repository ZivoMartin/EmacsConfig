;;; init-latex.el --- Minimalist LaTeX configuration for Emacs
;;; Commentary:
;;; Code:

;; Enable AUCTeX
(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t))

;; Syntax highlighting is built-in for LaTeX-mode

;; Company completion for LaTeX
(use-package company
  :ensure t
  :hook (LaTeX-mode . company-mode))

(use-package company-auctex
  :ensure t
  :after (company tex)
  :config
  (company-auctex-init))

(provide 'init-latex)
;;; init-latex.el ends here
