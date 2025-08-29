;;; init-pdf.el --- PDF configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal setup for viewing and annotating PDFs in Emacs using pdf-tools.

;;; Code:

;; Ensure pdf-tools is installed and initialized
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install) ;; Initialize pdf-tools
  (setq-default pdf-view-display-size 'fit-page)
  ;; Open PDFs in pdf-view mode by default
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

;; Convenience keybindings
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "n")   'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "p")   'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "g")   'pdf-view-goto-page))

(provide 'init-pdf)

;;; init-pdf.el ends here
