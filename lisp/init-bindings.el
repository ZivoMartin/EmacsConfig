;;; init-bindings.el --- Custom key bindings as an activatable minor mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This merges normal bindings and vterm bindings.
;; On conflict, dispatch based on whether we are in vterm-mode.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subword)
(require 'org)
(require 'vterm)
(require 'projectile)
;;;; Helpers & state ---------------------------------------------------------

(defvar-local martin--subword-was-active nil
  "Whether `subword-mode' was active before enabling `martin-mode'.")

(defun swap-line-up ()
  "Swap current line with the one above."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun swap-line-down ()
  "Swap current line with the one below."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(defun swap-word-left ()
  "Swap current word with the one on the left."
  (interactive)
  (transpose-words -1))

(defun swap-word-right ()
  "Swap current word with the one on the right."
  (interactive)
  (transpose-words 1))

(defun my-scroll-up () (interactive) (scroll-up-line 5))
(defun my-scroll-down () (interactive) (scroll-down-line 5))
(defun my/insert-space-no-move () (interactive) (save-excursion (insert " ")))
(defun my/toggle-comment-on-line ()
  "Comment or uncomment region, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      ;; Use region
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; Fallback to current line
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-beginning-position 2))))

(defun my/eglot-keybindings ()
  "Custom keybindings for Eglot-enabled buffers."
  (local-set-key (kbd "C-<return>") #'xref-find-definitions)
  (local-set-key (kbd "C-c C-b")    #'xref-go-back))
(add-hook 'eglot-managed-mode-hook #'my/eglot-keybindings)

;; Helper for vterm binding dispatch
(defun my/vterm-dispatch (emacs-fn vterm-key &optional shift meta ctrl)
  "Call VTERM-KEY in vterm/vterm-copy-mode or EMACS-FN otherwise."
  (lambda ()
    (interactive)
    (if (and (derived-mode-p 'vterm-mode)
             (not (bound-and-true-p vterm-copy-mode)))
        (vterm-send-key vterm-key shift meta ctrl)
      (call-interactively emacs-fn))))

(defun my/org-or-nothing (f)
  "Return a command that calls F if in Org mode, or does nothing otherwise."
  (lambda ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (funcall f))))

(defun my-beginning-of-line ()
  "Move to the first non-whitespace character."
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      (beginning-of-line))))

(defun copy-whole-line ()
  "Copy the current line to the kill ring without deleting it."
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-end-position)))

(declare-function my/vterm-find-file "init-vterm")

(defun my/find-file ()
  "Call `my/vterm-find-file` if in vterm, else `find-file`."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (my/vterm-find-file)
    (call-interactively #'find-file)))


(defvar martin--quote-pairs
  '((?\" . ?\")
    (?'  . ?'))
  "List of quote pairs handled by `martin-delete-pair-smart`.
Entries should be (OPENING . CLOSING).")


(defun martin--escaped-p (pos)
  "Return non-nil if the character at POS is escaped with a backslash."
  (when (and (> pos (point-min))
             (eq (char-before pos) ?\\))
    ;; Ensure backslash itself is not escaped (i.e., odd number of \)
    (let ((count 0)
          (p (1- pos)))
      (while (and (>= p (point-min))
                  (eq (char-before (1+ p)) ?\\))
        (setq count (1+ count))
        (setq p (1- p)))
      ;; Odd number of backslashes → escaped
      (oddp count))))


(defun martin--delete-quote-pair-at (pos)
  "Try to delete quotes around POS.
Supports escaped quotes. Returns t on success."
  (when (and (>= pos (point-min)) (< pos (point-max)))
    (let* ((char (char-after pos))
           (char-before (char-before pos))
           (pair-open  (car (assoc char martin--quote-pairs)))
           (pair-close (cdr (assoc char martin--quote-pairs))))
      (cond
       ;; On opening quote (ensure not escaped)
       ((and pair-open (not (martin--escaped-p pos)))
        (let ((end (save-excursion
                     (goto-char (1+ pos))
                     (catch 'found
                       (while (search-forward (char-to-string pair-close) nil t)
                         (unless (martin--escaped-p (point))
                           (throw 'found (point))))
                       nil))))
          (when end
            ;; remove closing
            (save-excursion
              (goto-char (1- end))
              (delete-char 1))
            ;; remove opening
            (delete-char 1)
            t)))

       ;; On closing quote (ensure not escaped)
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
            ;; remove opening
            (save-excursion
              (goto-char start)
              (delete-char 1))
            ;; remove closing
            (delete-char -1)
            t)))))))


(defun martin--delete-bracket-pair-at (pos)
  "Try to delete (), [], {}, <> pair at POS. Returns t on success."
  (when (and (>= pos (point-min)) (< pos (point-max)))
    (save-excursion
      (goto-char pos)
      (cond
       ;; opening delimiter
       ((memq (char-after) '(?\( ?\[ ?\{ ?<))
        (let ((end (scan-lists pos 1 0)))
          (when end
            (save-excursion
              (goto-char (1- end))
              (delete-char 1))
            (delete-char 1)
            t)))

       ;; closing delimiter
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
         ;; Try point → left → right
         (result (or (martin--delete-pair-at pos)
                     (martin--delete-pair-at (1- pos))
                     (martin--delete-pair-at (1+ pos)))))
    (unless result
      (message "No matching pair around point."))))


;;;; Keymap -----------------------------------------------------------------

(defvar martin-mode-map
  (let ((map (make-sparse-keymap)))

    ;; --------------------
    ;; Movements (conflicts handled)
    ;; --------------------
    (define-key map (kbd "C-;")
                (my/vterm-dispatch #'forward-char "<right>"))
    (define-key map (kbd "C-j")
                (my/vterm-dispatch #'backward-char "<left>"))
    (define-key map (kbd "C-k") #'next-line)
    (define-key map (kbd "C-l") #'previous-line)

    (define-key map (kbd "M-j")
                (my/vterm-dispatch #'subword-backward "<left>" nil nil t))
    (define-key map (kbd "M-;")
                (my/vterm-dispatch #'subword-forward "<right>" nil nil t))
    
    (define-key map (kbd "M-l") 
                (my/vterm-dispatch #'backward-paragraph "<up>"))
    (define-key map (kbd "M-k")
                (my/vterm-dispatch #'forward-paragraph "<down>"))

    (define-key map (kbd "C-M-;") #'forward-sexp)
    (define-key map (kbd "C-M-j") #'backward-sexp)    
    (define-key map (kbd "M-a") #'my-beginning-of-line)

    ;; Upper / lower
    (define-key map (kbd "C-u") #'downcase-word)
    (define-key map (kbd "M-u") #'upcase-word)
    (define-key map (kbd "C-M-u") #'capitalize-word)

    ;; Zoom
    (define-key map (kbd "C-=") #'text-scale-increase)
    (define-key map (kbd "C--") #'text-scale-decrease)

    ;; Saving
    (define-key map (kbd "C-z") #'save-buffer)
    (define-key map (kbd "C-x s") #'ignore)
    (define-key map (kbd "C-x C-s") #'ignore)

    ;; Goto
    (define-key map (kbd "M-g M-g") #'goto-line)
    (define-key map (kbd "M-g M-f") #'move-to-column)
    (define-key map (kbd "M-[") #'beginning-of-buffer)
    (define-key map (kbd "M-{") #'end-of-buffer)

    
    (use-package expand-region)
    (require 'expand-region)
    (define-key map (kbd "M-SPC") 'er/expand-region)
    (define-key map (kbd "M-C-SPC") 'er/contract-region)


    ;; Deletion (conflicts handled)
    (dolist (key '("DEL" "C-DEL" "M-DEL"
                   "<deletechar>" "<backspace>"
                   "C-<deletechar>" "M-<deletechar>"
                   "C-<backspace>"  "M-<backspace>"))
      (define-key map (kbd key) #'ignore))

    (define-key map (kbd "C-p")
                (my/vterm-dispatch #'delete-backward-char "<backspace>"))
    (define-key map (kbd "M-p")
                (my/vterm-dispatch #'subword-backward-kill "w" nil nil t))
    (define-key map (kbd "C-S-p")
                (my/vterm-dispatch #'backward-kill-word "w" nil nil t))
    (define-key map (kbd "C-'")
                (my/vterm-dispatch #'kill-line "k" nil nil t))
    (define-key map (kbd "M-'") #'kill-whole-line)
    (define-key map (kbd "M-C-'") #'copy-whole-line)
    

    ;; Caps mapping
    (define-key map (kbd "<capslock>") #'event-apply-control-modifier)

    ;; Window/frame (conflicts handled)
    (define-key map (kbd "<return>")
                (if (> (length (display-monitor-attributes-list)) 1)
                    #'other-frame
                  #'other-window))
    
    (define-key map (kbd "M-o") #'other-window)
    (define-key map (kbd "C-`") #'delete-window)
    (define-key map (kbd "C-2") #'split-window-right)
    (define-key map (kbd "C-3") #'split-window-below)
    (define-key map (kbd "C-1") #'delete-other-windows)

    ;; Remove old window bindings
    (dolist (k '("C-x 0" "C-x 1" "C-x 2" "C-x 3"))
      (define-key map (kbd k) #'ignore))

    ;; Arrows
    (define-key map (kbd "<up>")
                (my/vterm-dispatch #'swap-line-up "<up>" nil nil nil))
    (define-key map (kbd "<down>")
                (my/vterm-dispatch #'swap-line-down "<down>" nil nil nil))
    (define-key map (kbd "<left>")
                (my/vterm-dispatch #'undo-fu-only-undo "<left>" nil nil nil))
    (define-key map (kbd "<right>")
                (my/vterm-dispatch #'undo-fu-only-redo "<right>" nil nil nil))

    ;; Scrolling & buffer nav
    (define-key map (kbd "C-M-k") #'my-scroll-up)
    (define-key map (kbd "C-M-l") #'my-scroll-down)

    (define-key map (kbd "C-S-SPC") #'my/insert-space-no-move)
    (define-key map (kbd "C-,")     #'recenter-top-bottom)
    (define-key map (kbd "C-c h")   #'help-command)
    (define-key map (kbd "C-r")     #'query-replace)
    (define-key map (kbd "M-r")     #'replace-string)


    ;; Vterm management
    (declare-function my/vterm-split-right "init-vterm")
    (declare-function my/vterm-new-buffer "init-vterm")
    (declare-function my/open-emacs-on-laptop "init-vterm")
    (define-key map (kbd "C-c t") #'my/vterm-split-right)
    (define-key map (kbd "C-c n") #'my/vterm-new-buffer)
    (define-key map (kbd "C-c C-t") #'my/open-emacs-on-laptop)
    (define-key map (kbd "C-t") #'vterm-copy-mode)

    (define-key map (kbd "C-f") #'my/find-file)
    (define-key map (kbd "C-c C-f") #'projectile-find-file)

    (define-key map (kbd "C-b") #'switch-to-buffer)
    (define-key map (kbd "C-x C-f") #'my/find-file)
    (define-key map (kbd "C-9") #'rename-file)

    ;; Projectile
    (define-key map (kbd "s-g") #'projectile-grep)
    (define-key map (kbd "M-e") #'projectile-switch-project)

    ;; Org
    (define-key map (kbd "C-n") (my/org-or-nothing #'org-todo))
    (define-key map (kbd "M-n") (my/org-or-nothing #'org-insert-heading))
    
    (define-key map (kbd "M-q") #'org-agenda-list)
    (define-key map (kbd "M-t") #'org-todo-list)


    ;; Magit
    (define-key map (kbd "C-x C-g") #'magit-status)
    (global-set-key (kbd "C-c C-s") #'magit-stash)

    ;; Multi-cursor
    (define-key map (kbd "C-S-k")  #'mc/mark-next-like-this)
    (define-key map (kbd "C-S-l")  #'mc/mark-previous-like-this)

    (define-key map (kbd "C-:") #'mc/mark-next-like-this)
    (define-key map (kbd "C-S-j") #'mc/mark-previous-like-this)    

    (define-key map (kbd "M-<left>")  #'mc/mark-all-like-this)
    (define-key map (kbd "M-<down>")  #'set-rectangular-region-anchor)

    (define-key map (kbd "M-/") #'delete-horizontal-space)

    ;;; Indent
    (with-eval-after-load 'indent)
    (define-key map (kbd "C-<tab>") #'indent-rigidly)
    (define-key indent-rigidly-map (kbd "C-j") #'indent-rigidly-left)
    (define-key indent-rigidly-map (kbd "C-;") #'indent-rigidly-right)

    (define-key map (kbd "C-c C-d") #'martin-delete-pair-smart)

    ;; Region helpers
    (dolist (k '("C-x p" "C-x C-p"))
      (define-key map (kbd k)
                  (lambda () (interactive)
                    (execute-kbd-macro (kbd "C-SPC C-a C-p")))))

    ;; Commenting
    (define-key map (kbd "C-.") #'my/toggle-comment-on-line)
    map)
  "Keymap for `martin-mode'.")


;;;; Minor modes -------------------------------------------------------------
(define-minor-mode martin-mode
  "Toggle personal keybindings in the current buffer."
  :init-value nil
  :lighter " ⌨"
  :keymap martin-mode-map
  (if martin-mode
      (subword-mode 1)
    (subword-mode 0)))

(define-globalized-minor-mode martin-global-mode
  martin-mode
  (lambda () (martin-mode 1))
  :group 'convenience)

(martin-global-mode 1)

(provide 'init-bindings)
;;; init-bindings.el ends here
