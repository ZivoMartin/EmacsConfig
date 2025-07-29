;;; init-miscellaneous.el --- Miscellaneous editor behavior tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Small tweaks to improve the user experience:
;; - Use `y/n` instead of `yes/no` prompts
;; - Disable lockfile creation

;;; Code:

;; Use y/n instead of yes/no in prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't create lockfiles like .#filename
(setq create-lockfiles nil)

(provide 'init-miscellaneous)
;;; init-miscellaneous.el ends here
