;;; init-delete-pair.el --- Small package to delete matching parenthesis/quotes... -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar martin--quote-pairs
  '((?\" . ?\")
    (?'  . ?'))
  "List of quote pairs handled by `martin-delete-pair-smart`.
Entries should be (OPENING . CLOSING).")


(defun martin--escaped-p (pos)
  "Return non-nil if the character at POS is escaped with a backslash."
  (when (and (> pos (point-min))
             (eq (char-before pos) ?\\))

    (let ((count 0)
          (p (1- pos)))
      (while (and (>= p (point-min))
                  (eq (char-before (1+ p)) ?\\))
        (setq count (1+ count))
        (setq p (1- p)))

      (oddp count))))

(defun martin--delete-quote-pair-at (pos)
  "Try to delete quotes around POS.
Supports escaped quotes.  Return t on success."
  (when (and (>= pos (point-min)) (< pos (point-max)))
    (let* ((char (char-after pos))
           (char-before (char-before pos))
           (pair-open  (car (assoc char martin--quote-pairs)))
           (pair-close (cdr (assoc char martin--quote-pairs))))
      (cond

       ((and pair-open (not (martin--escaped-p pos)))
        (let ((end (save-excursion
                     (goto-char (1+ pos))
                     (catch 'found
                       (while (search-forward (char-to-string pair-close) nil t)
                         (unless (martin--escaped-p (point))
                           (throw 'found (point))))
                       nil))))
          (when end

            (save-excursion
              (goto-char (1- end))
              (delete-char 1))

            (delete-char 1)
            t)))

       ((and char-before
             (setq pair-open (car (rassoc char-before martin--quote-pairs)))
             (not (martin--escaped-p (1- pos))))
        (let ((start (save-excursion
                       (goto-char (1- pos))
                       (catch 'found
                         (while (search-backward (char-to-string pair-open) nil t)
                           (unless (martin--escaped-p (point))
                             (throw 'found (point))))
                         nil))))
          (when start

            (save-excursion
              (goto-char start)
              (delete-char 1))

            (delete-char -1)
            t)))))))


(defun martin--delete-bracket-pair-at (pos)
  "Try to delete (), [], {}, <> pair at POS.  Return t on success."
  (when (and (>= pos (point-min)) (< pos (point-max)))
    (save-excursion
      (goto-char pos)
      (cond

       ((memq (char-after) '(?\( ?\[ ?\{ ?<))
        (let ((end (scan-lists pos 1 0)))
          (when end
            (save-excursion
              (goto-char (1- end))
              (delete-char 1))
            (delete-char 1)
            t)))

       ((memq (char-before) '(?\) ?\] ?\} ?>))
        (let ((start (scan-lists pos -1 0)))
          (when start
            (save-excursion
              (goto-char start)
              (delete-char 1))
            (delete-char -1)
            t)))))))


(defun martin--delete-pair-at (pos)
  "Try deleting any pair (quotes or brackets) at POS."
  (or (martin--delete-bracket-pair-at pos)
      (martin--delete-quote-pair-at pos)))


(defun martin-delete-pair-smart ()
  "Delete matching pair around point.
Supports brackets, quotes, escaped quotes, and retries on neighbor chars."
  (interactive)
  (let* ((pos (point))
         (result (or (martin--delete-pair-at pos)
                     (martin--delete-pair-at (1- pos))
                     (martin--delete-pair-at (1+ pos)))))
    (unless result
      (message "No matching pair around point."))))

(provide 'init-delete-pair)
;;; init-delete-pair.el ends here
