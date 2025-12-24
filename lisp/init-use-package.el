;;; init-use-package.el --- Configure use-package -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(provide 'init-use-package)
;;; init-use-package.el ends here
