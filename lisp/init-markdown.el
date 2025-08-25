;;; init-markdown.el --- Markdown editing configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file configures Emacs for editing Markdown files.
;; It loads `markdown-mode` and optionally enables preview functionality.

;;; Code:

;; Ensure markdown-mode is available
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")  ;; Associate .md and .markdown files with markdown-mode
  :init
  (setq markdown-command "pandoc")     ;; Use pandoc for rendering if installed
  :config
  ;; Optional: auto-fill mode for long paragraphs
  (add-hook 'markdown-mode-hook #'auto-fill-mode)

  ;; Optional: enable visual-line-mode for better text wrapping
  (add-hook 'markdown-mode-hook #'visual-line-mode)

  ;; Optional: syntax highlighting for code blocks using `markdown-fontify-code-blocks-natively`
  (setq markdown-fontify-code-blocks-natively t))

;; markdown-preview-mode (easy to use)
(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("p" . markdown-preview-mode)))  ;; Press C-c C-c p to preview


(provide 'init-markdown)
;;; init-markdown.el ends here
