;;; init-pdf.el --- Defines all the pdf preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "n")   'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "p")   'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "g")   'pdf-view-goto-page))

(setq pdf-view-use-scaling t)
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(provide 'init-pdf)
;;; init-pdf.el ends here
