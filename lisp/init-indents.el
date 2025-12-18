;;; init-indents.el --- Defines all the indents preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'indent
  (keymap-set indent-rigidly-map "C-j" 'indent-rigidly-left)
  (keymap-set indent-rigidly-map "C-;" 'indent-rigidly-right))

(provide 'init-indents)
;;; init-indents.el ends here
