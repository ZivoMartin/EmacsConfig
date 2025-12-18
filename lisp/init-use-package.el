;;; init-use-package.el --- Configure use-package -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(require 'use-package)

(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/") t)
(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(provide 'init-use-package)
;;; init-use-package.el ends here
