;;; cr-mode.el --- Crystal editing config

;;; Commentary:
;; Crystal language specific things

;;; Code:

;;; Things we need
(require 'semantic)

;; Creating Syntax Table
(defconst cr-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Shamelessly imported from ruby-mode
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    table
    )
  )

;; Print shit
(defun debug_lexer (&rest things)
  "Print THINGS to a temp buffer called *lexer*."
  (with-output-to-temp-buffer "*lexer*"
    (apply 'pp things)
    )
  )

(define-lex semantic-crystal-lexer
  "Lexer for Crystal major mode."
  semantic-lex-ignore-comments
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-number
  semantic-lex-string
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren

  semantic-lex-punctuation
  semantic-lex-default-action
  )


(defun lex-region () "Lex the current region." (interactive)
  (debug_lexer
   (semantic-lex (region-beginning) (region-end) 0)
   )
  )

(global-set-key "\C-c\C-c" 'lex-region)

;; Syntax Highlighting
(setq cr-keywords '("class" "module" "class" "struct" "lib"
                    "def" "macro" "do" "end" "return"
                    "if" "else" "elsif" "while"
                    "case" "when" ) )
(setq cr-builtins '("require" "spawn" "puts" "sleep"
                    "pointerof" "sizeof" "instance_sizeof"))
(setq cr-types '("nil" "true" "false"))
(setq cr-preproc '("{{" "}}" "{%" "%}" ))

(setq cr-symbol-regex '"\<:@?@?[a-zA-Z0-9\_]+[!?]?\>")
(setq cr-keywords-regex (regexp-opt cr-keywords 'words))
(setq cr-builtins-regex (regexp-opt cr-builtins 'words))
(setq cr-types-regex (regexp-opt cr-types 'words))
(setq cr-preproc-regex (regexp-opt cr-preproc 'symbol))
(setq cr-number-suffix "\\(_\\([ui]\\(8\\|16\\)\\|[uif]\\(32\\|64\\)\\)\\)")
(setq cr-number-regex
      (
       eval-when-compile
        (concat "\\("
                 ;; 3.14159265
                 "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\(_f\\(32\\|64\\)\\)?\\>"
                 ;; 2e10_u64
                 "\\|"
                 "\\<[0-9]+[eE][-+]?[0-9]+"
                   cr-number-suffix
                 "?\\>"
                 ;; 0xff_u8 | 0o777_u8
                 "\\|"
                 "\\<0[xo][0-9a-fA-F]+"
                   cr-number-suffix
                 "?\\>"
                 ;; 128_f32
                 "\\|"
                 "\\<[0-9]+"
                   cr-number-suffix
                 "?\\>"
                 ;; 'c'
                 "\\|"
                 "'.+'"
                 "\\)"
                 ))

      )

(message cr-number-regex)

(setq cr-mode-font-lock
      `(
        (,cr-keywords-regex . font-lock-keyword-face)
        (,cr-builtins-regex . font-lock-builtin-face)
        (,cr-types-regex . font-lock-type-face)
        (,cr-preproc-regex . font-lock-preprocessor-face)
        (,cr-symbol-regex . font-lock-constant-face)
        ))

(defun semantic-crystal-setup ()
  "Setup a buffer for Semantic Parser of the Crystal language."
  (semantic-lex-init)
  (setq
   semantic-lex-analyzer 'semantic-crystal-lexer
   semantic-lex-number-expression cr-number-regex
   ;; semantic-lex-debug t
   )
  )


;; Creating Mode
(define-derived-mode cr-mode prog-mode "Crystal Language Mode"
  "Some kind of mode I'm trying to write with much effort to support crystal
   in emacs"
  :syntax-table cr-mode-syntax-table
  (setq font-lock-defaults '(cr-mode-font-lock))
  (font-lock-ensure)
  )

(add-hook 'cr-mode-hook 'semantic-crystal-setup)

(add-to-list 'auto-mode-alist '("\\.cr\\'" . cr-mode))

(setq
 debug-on-error t
 debug-on-message "Wrong.*"
 )
;; add the cd-mode to the `features' list
(provide 'cr-mode)

;;; cr-mode.el ends here
