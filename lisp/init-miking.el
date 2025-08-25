;;; init-miking.el --- Minimal Miking mode for .mc files -*- lexical-binding: t; -*-

;; Basic, fast syntax highlighting for the Miking language:
;; - Keywords
;; - Types (identifiers starting with a capital letter)
;; - Names right after `let` (optionally `recursive`) and `lam`
;; - Strings
;; - Comments:
;;     -- single-line
;;     /- ... -/ multi-line

(defgroup miking nil
  "Major mode for the Miking programming language."
  :group 'languages)

(defvar miking-mode-hook nil
  "Hook run when entering `miking-mode'.")

;; ---------- Syntax table ----------
(defvar miking-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Strings: " ... "
    (modify-syntax-entry ?\" "\"" st)

    ;; Treat underscore as a word constituent.
    (modify-syntax-entry ?_ "w" st)

    ;; Default: everything else punctuation unless overridden by font-lock.
    st)
  "Syntax table for `miking-mode'.")

;; ---------- Font-lock (highlighting) ----------
(defconst miking--keywords
  '("let" "in" "if" "then" "else" "type" "match" "with" "recursive" "include"
    "sem" "syn" "lang" "end" "utest" "mexpr" "use" "con" "switch" "case" "lam"))

(defconst miking--keywords-regexp
  (regexp-opt miking--keywords 'symbols))

;; Identifier = word or symbol constituent
(defconst miking--ident "\\(?:\\sw\\|\\s_\\)+")

(defconst miking-font-lock-keywords
  `(
    ;; COMMENTS (put first so they override other faces)
    ;; Multi-line: /- ... -/
    ("/-\\(?:.\\|\n\\)*?-/" . (0 font-lock-comment-face t))
    ;; Single-line: -- ...
    ("--.*$" . (0 font-lock-comment-face t))

    ;; KEYWORDS
    (,miking--keywords-regexp . font-lock-keyword-face)

    ;; TYPE NAMES: Capitalized identifiers (e.g., `List`, `TreeNode`)
    ("\\_<[A-Z][[:word:]]*\\_>" . font-lock-type-face)

    ;; NAMES right after `let` (optionally `recursive`)
    (,(concat "\\_<let\\_>\\s-+\\(?:recursive\\s-+\\)?\\(" miking--ident "\\)")
     (1 font-lock-variable-name-face))

    ;; NAME right after `lam`
    (,(concat "\\_<lam\\_>\\s-+\\(" miking--ident "\\)")
     (1 font-lock-variable-name-face))
    ))

;; ---------- Major mode definition ----------
;;;###autoload
(define-derived-mode miking-mode prog-mode "Miking"
  "A minimal major mode for the Miking programming language."
  :syntax-table miking-mode-syntax-table
  (setq-local case-fold-search nil)        ; be strict about case
  (setq-local font-lock-multiline t)       ; allow multi-line comment regex
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--+\\s-*")

  ;; Use our font-lock keywords; keep syntactic fontification for strings.
  (setq-local font-lock-defaults
              '(miking-font-lock-keywords
                nil                          ; KEYWORDS-ONLY? (nil = also do strings/comments syntactically)
                nil                          ; CASE-FOLD? (nil = use buffer-local case-fold-search)
                ((?_ . "w"))))               ; treat underscore as word constituent
  ;; Comment helpers (so M-; uses `--`; block comments still highlighted via font-lock)
  (setq-local comment-end "")
  (setq-local comment-use-syntax nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mc\\'" . miking-mode))

(provide 'init-miking)
;;; init-miking.el ends here
