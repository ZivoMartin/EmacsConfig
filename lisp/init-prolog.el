;;; init-prolog.el --- Simple Prolog configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal and reliable Prolog setup.
;; Uses only built-in Emacs features:
;; - prolog-mode
;; - syntax highlighting
;; - flymake error highlighting
;; - optional indentation on save

;;; Code:

;; Ensure Prolog files use prolog-mode
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; Basic Prolog configuration
(with-eval-after-load 'prolog
  ;; Use ISO Prolog indentation style
  (setq prolog-indent-width 4)

  ;; Enable Flymake for error highlighting (built-in)
  (add-hook 'prolog-mode-hook #'flymake-mode)

  ;; Optional: automatically indent buffer on save
  ;; This is safe and does NOT reformat aggressively
  (defun my/prolog-indent-on-save ()
    "Indent Prolog buffer before saving."
    (when (eq major-mode 'prolog-mode)
      (indent-region (point-min) (point-max))))

  ;; Comment this line if you do NOT want formatting on save
  (add-hook 'before-save-hook #'my/prolog-indent-on-save))

(provide 'init-prolog)
;;; init-prolog.el ends here
