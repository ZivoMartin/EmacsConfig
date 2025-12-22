;;; init-multi-cursor.el --- Defines all the multi-cursor preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(use-package multiple-cursors
  :config
  (setq mc/always-repeat-command t)
  (setq mc/always-run-for-all t))

(provide 'init-multi-cursor)
;;; init-multi-cursor.el ends here
