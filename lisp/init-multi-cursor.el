;;; init-multi-cursor.el --- Defines all the multi-cursor preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package multiple-cursors
  :config
  (keymap-set mc/keymap "C-'" nil)
  (setq mc/always-repeat-command t)
  (setq mc/always-run-for-all t))

(provide 'init-multi-cursor)
;;; init-multi-cursor.el ends here
