;;; init-indents.el --- Defines all the indents preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'indent
  (keymap-set indent-rigidly-map "C-j" 'indent-rigidly-left)
  (keymap-set indent-rigidly-map "C-;" 'indent-rigidly-right))

(defun martin-force-tab-insert ()
  "Simply insert 4 spaces."
  (interactive)
  (insert "    "))

(provide 'init-indents)
;;; init-indents.el ends here
