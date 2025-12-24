;;; init-orderless.el --- Defines all the orderless preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

(provide 'init-orderless)
;;; init-orderless.el ends here
