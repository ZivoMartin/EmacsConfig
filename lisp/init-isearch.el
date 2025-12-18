;;; init-isearch.el --- Defines all the isearch preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'isearch
  (let ((map isearch-mode-map))
    (keymap-set map "M-s" 'isearch-repeat-backward)
    

    (keymap-set map "C-p" 'isearch-delete-char)
    (keymap-set map "C-g" 'isearch-exit)
    (keymap-set map "C-j" nil)
    ))

(provide 'init-isearch)
;;; init-isearch.el ends here
