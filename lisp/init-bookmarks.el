;;; init-bookmarks.el --- Defines all the c preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun martin-bookmark-set-auto ()
  "Set a bookmark named automatically as file:line:column."
  (interactive)
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (line (line-number-at-pos))
         (col (current-column))
         (name (format "%s:%d:%d"
                       (file-name-nondirectory file)
                       line
                       col)))
    (bookmark-set name)))

(defun martin-bookmark-jump-last ()
  "Jump to the most recently set bookmark."
  (interactive)
  (bookmark-jump (car bookmark-history)))

(provide 'init-bookmarks)
;;; init-bookmarks.el ends here
