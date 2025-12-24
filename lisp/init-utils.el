;;; init-utils.el --- Define some utils and interactive function -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subword)

(defun martin-scroll-up ()
  "Scroll up of 5 lines."
  (interactive)
  (scroll-up 5))

(defun martin-scroll-down ()
  "Scroll down of 5 lines."
  (interactive)
  (scroll-down 5))

(defun martin-downcase-whole-word ()
  "Downcase the symbol at point."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (downcase-region (car bounds) (cdr bounds))))))

(defun martin-upcase-whole-word ()
  "Downcase the symbol at point."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (upcase-region (car bounds) (cdr bounds))))))

(defun martin-capitalize-whole-word ()
  "Capitalize the symbol at point."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (capitalize-region (car bounds) (cdr bounds))))))

(defun martin-kill-line-left ()
  "Delete the left part of the line from the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (delete-region (car bounds) (point)))))

(defun martin-copy-line ()
  "Copy the whole line the cursor is currently on."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (message "Copied!")
      (kill-ring-save (car bounds) (cdr bounds)))))

(defun martin-copy-buffer ()
  "Copy the entire contents of the current buffer."
  (interactive)
  (message "Copied!")
  (kill-ring-save (point-min) (point-max)))

(defun martin-select-buffer ()
  "Select the entire buffer without visually moving point."
  (interactive)
  (let ((pos (point)))
    (mark-whole-buffer)
    (goto-char pos)))

(defun kill-symbol-at-point ()
  "Kill the whole the cursor is currently on."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (kill-region (car bounds) (cdr bounds)))))

(defun martin-insert-space ()
  "Insert a space at the current position without moving the cursor."
  (interactive)
  (save-excursion
    (insert " ")))

(defun martin-previous-window ()
  "Like `other-window` but with the non cyclic order."
  (interactive)
  (other-window -1))

(defun martin-swap-line-up ()
  "Swap current line with the one above."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun martin-swap-line-down ()
  "Swap current line with the one below."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))



(provide 'init-utils)
;;; init-utils.el ends here
