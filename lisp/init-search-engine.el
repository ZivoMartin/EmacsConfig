;;; init-search-engine.el --- Defines all the search-engine preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(use-package vertico
  :config
  (vertico-mode 1)
  (vertico-grid-mode 1)
  (setq vertico-grid-min-columns 1
        vertico-grid-max-columns 3)
  (setq vertico-indexed-start 1))

(provide 'init-search-engine)
;;; init-search-engine.el ends here
