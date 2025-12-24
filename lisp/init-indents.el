;;; init-indents.el --- Defines all the indents preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'indent
  (keymap-set indent-rigidly-map "C-j" 'indent-rigidly-left)
  (keymap-set indent-rigidly-map "C-;" 'indent-rigidly-right))

(defun martin-force-tab-insert ()
  "Simply insert 4 spaces."
  (interactive)
  (insert "    "))

(setq-default indent-tabs-mode nil)  ;; Use spaces, no tab
(setq-default tab-width 4)           ;; Tabs are 4 espaces longs
(setq-default c-basic-offset 4)      ;; Indentation size while programming
(setq c-default-style "linux")       ;; Indentation style

(provide 'init-indents)
;;; init-indents.el ends here
