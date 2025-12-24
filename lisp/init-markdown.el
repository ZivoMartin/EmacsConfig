;;; init-markdown.el --- Defines all the markdown preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package markdown-preview-mode
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("p" . markdown-preview)))

(provide 'init-markdown)
;;; init-markdown.el ends here
