;;; crystal-wy.el --- Generated parser support file

;; Copyright (C) 2017 Julien 'Lta' BALLET

;; Author: Lta <lta@NightCrawler>
;; Created: 2017-05-21 19:40:57+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file crystal.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;
(require 'cr-regexps)

;;; Declarations
;;
(defconst wisent-crystal-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("class" . CLASS)
     ("def" . DEF)
     ("do" . DO)
     ("enum" . ENUM)
     ("lib" . LIB)
     ("macro" . MACRO)
     ("module" . MODULE)
     ("struct" . STRUCT)
     ("begin" . BEGIN)
     ("break" . BREAK)
     ("case" . CASE)
     ("else" . ELSE)
     ("elsif" . ELSIF)
     ("ensure" . ENSURE)
     ("for" . FOR)
     ("if" . IF)
     ("rescue" . RESCUE)
     ("return" . RETURN)
     ("when" . WHEN)
     ("while" . WHILE)
     ("until" . UNTIL)
     ("unless" . UNLESS)
     ("yield" . YIELD)
     ("next" . NEXT)
     ("end" . END)
     ("abstract" . ABSTRACT)
     ("extend" . EXTEND)
     ("private" . PRIVATE)
     ("protected" . PROTECTED)
     ("public" . PUBLIC)
     ("fun" . FUN)
     ("include" . INCLUDE)
     ("require" . REQUIRE)
     ("out" . OUT)
     ("of" . OF)
     ("uninitialized" . UNINITIALIZED))
   'nil)
  "Table of language keywords.")

(defconst wisent-crystal-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUMBER))
     ("string"
      (STRING))
     ("libattr"
      (LIBATTR))
     ("global"
      (GLOBAL))
     ("constant"
      (CID))
     ("wordlist"
      (WORDLIST))
     ("identifier"
      (ID)
      (NIL . "nil")
      (TRUE . "true")
      (FALSE . "false"))
     ("symbol"
      (SYM))
     ("punctuation"
      (XOREQ . "^=")
      (XOR . "^")
      (TILDEEQ . "~=")
      (TILDE . "~")
      (SEMICOLON . ";")
      (RSHIFTEQ . ">>=")
      (RSHIFT . ">>")
      (RANGE3 . "...")
      (RANGE . "..")
      (PLUSEQ . "+=")
      (PLUS . "+")
      (OREQ . "|=")
      (OR . "|")
      (NTILDE . "!~")
      (NEQ . "!=")
      (MULTEQ . "*=")
      (MULT . "*")
      (MODEQ . "%=")
      (MOD . "%")
      (MINEQ . "-=")
      (MIN . "-")
      (LTEQ . "<=")
      (LT . "<")
      (LSHIFTEQ . "<<=")
      (LSHIFT . "<<")
      (LOGOREQ . "||=")
      (LOGOR . "||")
      (LOGNOT . "!!")
      (LOGANDEQ . "&&=")
      (LOGAND . "&&")
      (INC . "++")
      (GTEQ . ">=")
      (GT . ">")
      (EXPEQ . "**=")
      (EXP . "**")
      (EQ . "==")
      (EOL . "\n")
      (DOT . ".")
      (DIVEQ . "/=")
      (DIV . "/")
      (DIAMOND . "<=>")
      (DEC . "--")
      (COMMA . ",")
      (COLON . ":")
      (CASEEQ . "===")
      (AT . "@")
      (ARROW . "->")
      (ACCESS . "::")
      (ASSIGN . "=")
      (ANDEQ . "&=")
      (AND . "&")))
   '(("number" :declared t)
     ("string" :declared t)
     ("libattr" syntax cr-lib-attr-regex)
     ("libattr" matchdatatype regexp)
     ("libattr" :declared t)
     ("global" syntax cr-global-identifier-regex)
     ("global" matchdatatype regexp)
     ("global" :declared t)
     ("constant" syntax cr-constant-identifier-regex)
     ("constant" matchdatatype regexp)
     ("constant" :declared t)
     ("wordlist" syntax cr-wordlist-regex)
     ("wordlist" matchdatatype regexp)
     ("wordlist" :declared t)
     ("identifier" syntax cr-identifier-regex)
     ("identifier" matchdatatype regexp)
     ("identifier" :declared t)
     ("symbol" syntax cr-symbol-regex)
     ("symbol" matchdatatype regexp)
     ("symbol" :declared t)
     ("keyword" :declared t)
     ("punctuation" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-crystal-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((AND ANDEQ ASSIGN ACCESS ARROW AT CASEEQ COLON COMMA DEC DIAMOND DIV DIVEQ DOT EOL EQ EXP EXPEQ GT GTEQ INC LOGAND LOGANDEQ LOGNOT LOGOR LOGOREQ LSHIFT LSHIFTEQ LT LTEQ MIN MINEQ MOD MODEQ MULT MULTEQ NEQ NTILDE OR OREQ PLUS PLUSEQ RANGE RANGE3 RSHIFT RSHIFTEQ SEMICOLON TILDE TILDEEQ XOR XOREQ CLASS DEF DO ENUM LIB MACRO MODULE STRUCT BEGIN BREAK CASE ELSE ELSIF ENSURE FOR IF RESCUE RETURN WHEN WHILE UNTIL UNLESS YIELD NEXT END ABSTRACT EXTEND PRIVATE PROTECTED PUBLIC FUN INCLUDE REQUIRE OUT OF UNINITIALIZED SYM FALSE TRUE NIL ID WORDLIST CID GLOBAL LIBATTR STRING NUMBER)
       nil
       (program
	((EOL))))
     '(program)))
  "Parser table.")

(defun wisent-crystal-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-crystal-wy--parse-table
	semantic-debug-parser-source "crystal.wy"
	semantic-flex-keywords-obarray wisent-crystal-wy--keyword-table
	semantic-lex-types-obarray wisent-crystal-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-regex-type-analyzer wisent-crystal-wy--<libattr>-regexp-analyzer
  "regexp analyzer for <libattr> tokens."
  cr-lib-attr-regex
  nil
  'LIBATTR)

(define-lex-string-type-analyzer wisent-crystal-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((XOREQ . "^=")
    (XOR . "^")
    (TILDEEQ . "~=")
    (TILDE . "~")
    (SEMICOLON . ";")
    (RSHIFTEQ . ">>=")
    (RSHIFT . ">>")
    (RANGE3 . "...")
    (RANGE . "..")
    (PLUSEQ . "+=")
    (PLUS . "+")
    (OREQ . "|=")
    (OR . "|")
    (NTILDE . "!~")
    (NEQ . "!=")
    (MULTEQ . "*=")
    (MULT . "*")
    (MODEQ . "%=")
    (MOD . "%")
    (MINEQ . "-=")
    (MIN . "-")
    (LTEQ . "<=")
    (LT . "<")
    (LSHIFTEQ . "<<=")
    (LSHIFT . "<<")
    (LOGOREQ . "||=")
    (LOGOR . "||")
    (LOGNOT . "!!")
    (LOGANDEQ . "&&=")
    (LOGAND . "&&")
    (INC . "++")
    (GTEQ . ">=")
    (GT . ">")
    (EXPEQ . "**=")
    (EXP . "**")
    (EQ . "==")
    (EOL . "\n")
    (DOT . ".")
    (DIVEQ . "/=")
    (DIV . "/")
    (DIAMOND . "<=>")
    (DEC . "--")
    (COMMA . ",")
    (COLON . ":")
    (CASEEQ . "===")
    (AT . "@")
    (ARROW . "->")
    (ACCESS . "::")
    (ASSIGN . "=")
    (ANDEQ . "&=")
    (AND . "&"))
  'punctuation)

(define-lex-regex-type-analyzer wisent-crystal-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  cr-symbol-regex
  nil
  'SYM)

(define-lex-regex-type-analyzer wisent-crystal-wy--<wordlist>-regexp-analyzer
  "regexp analyzer for <wordlist> tokens."
  cr-wordlist-regex
  nil
  'WORDLIST)

(define-lex-regex-type-analyzer wisent-crystal-wy--<constant>-regexp-analyzer
  "regexp analyzer for <constant> tokens."
  cr-constant-identifier-regex
  nil
  'CID)

(define-lex-regex-type-analyzer wisent-crystal-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER)

(define-lex-regex-type-analyzer wisent-crystal-wy--<global>-regexp-analyzer
  "regexp analyzer for <global> tokens."
  cr-global-identifier-regex
  nil
  'GLOBAL)

(define-lex-regex-type-analyzer wisent-crystal-wy--<identifier>-regexp-analyzer
  "regexp analyzer for <identifier> tokens."
  cr-identifier-regex
  '((NIL . "nil")
    (TRUE . "true")
    (FALSE . "false"))
  'ID)

(define-lex-sexp-type-analyzer wisent-crystal-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING)

(define-lex-keyword-type-analyzer wisent-crystal-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;

(provide 'crystal-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; crystal-wy.el ends here
