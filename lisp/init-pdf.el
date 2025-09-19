;;; init-pdf.el --- PDF configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal setup for viewing and annotating PDFs in Emacs using pdf-tools.
;; Adds a helper to regenerate appearances for form fields (qpdf required).

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

;; Extra: regenerate appearances of filled PDF forms using qpdf
(defun my/pdf-generate-appearances ()
  "Regenerate appearances for the current PDF using qpdf, then reload."
  (interactive)
  (let* ((file (buffer-file-name))
         (tmp (make-temp-file "emacs-pdf-" nil ".pdf")))
    (unless (executable-find "qpdf")
      (user-error "qpdf not found in PATH"))
    (call-process "qpdf" nil nil nil file "--generate-appearances" tmp)
    (copy-file tmp file t) ;; overwrite original
    (kill-buffer)
    (find-file file)
    (message "Regenerated appearances with qpdf")))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-c C-a") #'my/pdf-generate-appearances))

(provide 'init-pdf)

;;; init-pdf.el ends here
