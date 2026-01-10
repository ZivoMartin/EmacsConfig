;;; init-ansi.el --- Defines all the ansi preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'ansi-color)

(defun martin-ansi-colorize-buffer ()
  "Apply ansi colors the buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-ansi)
;;; init-ansi.el ends here
