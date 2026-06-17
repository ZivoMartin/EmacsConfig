;;; init-troupe.el --- Simple troupe major mode -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.2

;;; Commentary:
;; Minimal major mode for the Troupe language.
;; Features:
;; - Keyword highlighting
;; - Builtin highlighting
;; - String highlighting
;; - Nestable ML-style comment highlighting (* ... *)

;;; Code:

(defvar troupe-mode-hook nil)

(defvar troupe-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `troupe-mode'.")

;; ------------------------
;; Syntax table
;; ------------------------

(defvar troupe-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Strings: "..."
    (modify-syntax-entry ?\" "\"" st)

    ;; Underscore is part of words
    (modify-syntax-entry ?_ "w" st)


    (modify-syntax-entry ?\( "()1nb" st)
    (modify-syntax-entry ?\* ". 23nb" st)
    (modify-syntax-entry ?\) ")(4nb" st)

    st)
  "Syntax table for `troupe-mode'.")

;; ------------------------
;; Font-lock keywords
;; ------------------------

(defconst troupe-keywords
  '("let" "in" "end" "fun" "val" "if" "then" "else" "case" "of" "import" "hn")
  "Language keywords for Troupe.")

(defconst troupe-builtins
  '("spawn" "send" "receive" "printString" "node" "self"
    "register" "whereis" "authority")
  "Built-in functions for Troupe.")

(defconst troupe-font-lock-keywords
  `(;; Keywords
    (,(regexp-opt troupe-keywords 'words) . font-lock-keyword-face)
    ;; Builtins
    (,(regexp-opt troupe-builtins 'words) . font-lock-builtin-face))
  "Font-lock keyword specification for `troupe-mode'.
Strings and comments are handled via the syntax table.")

;; ------------------------
;; Major mode definition
;; ------------------------

;;;###autoload
(define-derived-mode troupe-mode prog-mode "Troupe"
  "A major mode for the Troupe language."
  :syntax-table troupe-mode-syntax-table

  (setq-local font-lock-defaults '(troupe-font-lock-keywords nil nil nil nil))

  ;; Comment configuration (used by comment-dwim, fill-paragraph, etc.)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-style 'extra-line))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trp\\'"    . troupe-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.troupe\\'" . troupe-mode))

(provide 'init-troupe)

;;; init-troupe.el ends here
