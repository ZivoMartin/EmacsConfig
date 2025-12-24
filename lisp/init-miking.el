;;; init-miking.el --- Minimal Miking mode for .mc files -*- lexical-binding: t; -*-

(defgroup miking nil
  "Major mode for the Miking programming language."
  :group 'languages)

(defvar miking-mode-hook nil
  "Hook run when entering `miking-mode'.")

;; ---------- Syntax table ----------
(defvar miking-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)

    ;; Treat underscore as a word constituent.
    (modify-syntax-entry ?_ "w" st)

    ;; Default: everything else punctuation unless overridden by font-lock.
    st)
  "Syntax table for `miking-mode'.")

;; ---------- Font-lock (highlighting) ----------
(defconst miking--keywords
  '("let" "in" "if" "then" "else" "type" "match" "with" "recursive" "include"
    "sem" "syn" "lang" "end" "utest" "mexpr" "use" "con" "switch" "case" "lam" "using"))

(defconst miking--keywords-regexp
  (regexp-opt miking--keywords 'symbols))

;; Identifier = word or symbol constituent
(defconst miking--ident "\\(?:\\sw\\|\\s_\\)+")

(defconst miking-font-lock-keywords
  `(
    ("/-\\(?:.\\|\n\\)*?-/" . (0 font-lock-comment-face t))
    ("--.*$" . (0 font-lock-comment-face t))

    (,miking--keywords-regexp . font-lock-keyword-face)

    ("\\_<[A-Z][[:word:]]*\\_>" . font-lock-type-face)

    (,(concat "\\_<let\\_>\\s-+\\(?:recursive\\s-+\\)?\\(" miking--ident "\\)")
     (1 font-lock-variable-name-face))

    (,(concat "\\_<lam\\_>\\s-+\\(" miking--ident "\\)")
     (1 font-lock-variable-name-face))
    ))

(define-derived-mode miking-mode prog-mode "Miking"
  "A minimal major mode for the Miking programming language."
  :syntax-table miking-mode-syntax-table
  (setq-local case-fold-search nil)
  (setq-local font-lock-multiline t)
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--+\\s-*")

  (setq-local font-lock-defaults
              '(miking-font-lock-keywords
                nil
                nil
                ((?_ . "w"))))
  (setq-local comment-end "")
  (setq-local comment-use-syntax nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mc\\'" . miking-mode))

(provide 'init-miking)
;;; init-miking.el ends here
