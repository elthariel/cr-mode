;;; cr-mode.el --- Crystal editing config

;;; Commentary:
;; Crystal language specific things

;; (setq cr-number-regex-old
;;       (
;;        eval-when-compile
;;         (concat "\\("
;;                  ;; 3.14159265
;;                 "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\(_f\\(32\\|64\\)\\)?\\>"
;;                  ;; 2e10_u64
;;                  "\\|"
;;                  "\\<[0-9]+[eE][-+]?[0-9]+"
;;                    cr-number-suffix
;;                  "?\\>"
;;                  ;; 0xff_u8 | 0o777_u8
;;                  "\\|"
;;                  "\\<0[xo][0-9a-fA-F]+"
;;                    cr-number-suffix
;;                  "?\\>"
;;                  ;; 128_f32
;;                  "\\|"
;;                  "\\<[0-9]+"
;;                    cr-number-suffix
;;                  "?\\>"
;;                  ;; 'c'
;;                  "\\|"
;;                  "'.+'"
;;                  "\\)"
;;                  ))

;;       )

;;; Code:

;;; Things we need
(require 'semantic)
(require 'cr-regexps)
(require 'crystal-wy)

;; Creating Syntax Table
(defconst cr-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Shamelessly imported from ruby-mode
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n "." table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?: "." table)
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
  ;; semantic-lex-ignore-newline
  wisent-crystal-wy--<number>-regexp-analyzer
  wisent-crystal-wy--<string>-sexp-analyzer
  wisent-crystal-wy--<wordlist>-regexp-analyzer
  wisent-crystal-wy--<global>-regexp-analyzer
  wisent-crystal-wy--<libattr>-regexp-analyzer
  wisent-crystal-wy--<punctuation>-string-analyzer
  wisent-crystal-wy--<keyword>-keyword-analyzer
  wisent-crystal-wy--<symbol>-regexp-analyzer
  wisent-crystal-wy--<constant>-regexp-analyzer
  wisent-crystal-wy--<identifier>-regexp-analyzer
  ;; semantic-lex-symbol-or-keyword
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-default-action
  )


(defun lex-region () "Lex the current region." (interactive)
       (let* ((case-fold-search nil))
         (debug_lexer
          (semantic-lex (region-beginning) (region-end) 0)
          )))

(defun parse-region () "Parse the current region." (interactive)
       (let* ((case-fold-search nil))
         (debug_lexer
          ;; semantic--parse-table
          (wisent-parse semantic--parse-table
                        wisent-lexer-function
                        wisent-error-function
                        'program)
          )))


(global-set-key "\C-c\C-c" 'lex-region)
(global-set-key "\C-c\C-v" 'parse-region)


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
   semantic-case-fold t
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
  (setq font-lock-defaults '(cr-mode-font-lock)
        )
  (wisent-crystal-wy--install-parser)
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
