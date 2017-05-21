;;; cr-regexps.el --- Generated parser support file

;; Copyright (C) 2017 Julien 'Lta' BALLET

;; Author: Lta <lta@NightCrawler>
;; Created: 2017-05-21 15:13:11+0200
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
;; All the regexps used by cr-mode

;;; History:
;;

;;; Code:

(setq cr-keywords '("class" "module" "class" "struct" "lib"
                    "def" "macro" "do" "end" "return"
                    "if" "else" "elsif" "while"
                    "case" "when" ) )
(setq cr-builtins '("require" "spawn" "puts" "sleep"
                    "pointerof" "sizeof" "instance_sizeof"))
(setq cr-types '("nil" "true" "false"))
(setq cr-preproc '("{{" "}}" "{%" "%}" ))

(setq cr-symbol-regex '"\<:@?@?[a-zA-Z0-9_]+[!?]?\>")
(setq cr-keywords-regex (regexp-opt cr-keywords 'words))
(setq cr-builtins-regex (regexp-opt cr-builtins 'words))
(setq cr-types-regex (regexp-opt cr-types 'words))
(setq cr-preproc-regex (regexp-opt cr-preproc 'symbol))
(setq cr-number-suffix "\\(_\\([ui]\\(8\\|16\\)\\|[uif]\\(32\\|64\\)\\)\\)")
(setq cr-number-regex
      (eval-when-compile
        (concat
         "\\("
         "\\<-?[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\(_f\\(32\\|64\\)\\)?\\>"
         "\\|"
         "\\<-?[0-9]+[eE][-+]?[0-9]+" cr-number-suffix "?\\>"
         "\\|"
         "\\<0x[0-9a-fA-F]+" cr-number-suffix "?\\>"
         "\\|"
         "\\<0o[0-7]+" cr-number-suffix "?\\>"
         "\\|"
         "\\<-?[0-9]+" cr-number-suffix "?\\>"
         "\\|"
         "'[^'\n]+'"
         "\\)"
         )))
(setq cr-identifier-regex "[a-z_][a-zA-Z0-9_]*[!?]?")
(setq cr-constant-identifier-regex "\\<[A-Z][a-zA-Z0-9_]*[!?]?")
(setq cr-global-identifier-regex "\\$\\([~?]\\|[a-zA-Z0-9]+\\)")
(setq cr-wordlist-regex
      (eval-when-compile
        (concat
         "\\("
         "\\%[iqQrxw]\\[[^]]*\\]"
         "\\|"
         "\\%[iqQrxw]([^)]*)"
         "\\|"
         "\\%[iqQrxw]<[^>]*>"
         "\\|"
         "\\%[iqQrxw]{[^}]*}"
         "\\)"
         )))
(setq cr-lib-attr-regex "@\\[[^]]*\\]")

(provide 'cr-regexps)

;;; cr-regexps.el ends here
