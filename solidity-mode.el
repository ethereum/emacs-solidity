;;; solmode.el --- Major Emacs mode for ethereum solidity
;;; Commentary:
;;  As always for all Emacs modes:
;;  http://www.emacswiki.org/emacs/ModeTutorial
;;; Code:
(require 'cc-mode)
(defvar solidity-mode-hook nil)

(defvar solidity-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for solidity major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

(defconst solidity-keywords
  '("break"
    "case"
    "const"
    "continue"
    "contract"
    "default"
    "delete"
    "do"
    "else"
    "for"
    "function"
    "if"
    "in"
    "mapping"
    "new"
    "private"
    "public"
    "return"
    "returns"
    "struct"
    "switch"
    "this"
    "var"
    "while"
    "constant"
    )
  "Keywords of the solidity language.")

(defconst solidity-constants
  '("true" "false"
    "wei"
    "szabo"
    "finney"
    "ether")
  "Constants in the solidity language.")

(defconst solidity-builtin-types
  '("address" "bool"
    "hash" "hash8" "hash16" "hash24" "hash32" "hash40" "hash48" "hash56"
    "hash64" "hash72" "hash80" "hash88" "hash96" "hash104" "hash112" "hash120"
    "hash128" "hash136" "hash144" "hash152" "hash160" "hash168" "hash178"
    "hash184" "hash192" "hash200" "hash208" "hash216" "hash224" "hash224"
    "hash232" "hash240" "hash248" "hash256"
    "int"
    "int8"
    "int16"
    "int24"
    "int32"
    "int40"
    "int48"
    "int56"
    "int64"
    "int72"
    "int80"
    "int88"
    "int96"
    "int104"
    "int112"
    "int120"
    "int128"
    "int136"
    "int144"
    "int152"
    "int160"
    "int168"
    "int178"
    "int184"
    "int192"
    "int200"
    "int208"
    "int216"
    "int224"
    "int232"
    "int240"
    "int248"
    "int256"

    "mapping"
    "real"
    "string"
    "text"

    "uint"
    "uint8"
    "uint16"
    "uint24"
    "uint32"
    "uint40"
    "uint48"
    "uint56"
    "uint64"
    "uint72"
    "uint80"
    "uint88"
    "uint96"
    "uint104"
    "uint112"
    "uint120"
    "uint128"
    "uint136"
    "uint144"
    "uint152"
    "uint160"
    "uint168"
    "uint178"
    "uint184"
    "uint192"
    "uint200"
    "uint208"
    "uint216"
    "uint224"
    "uint232"
    "uint240"
    "uint248"
    "uint256"

    "msg"
    "block"
    "tx"

    "ureal")
  "Built in data types of the solidity language.")

(defvar solidity-identifier-regexp
  "\\([a-zA-z0-9]\\|_\\)+")

(defvar solidity-variable-attributes
  "\\(&\\|*\\|~\\)"
  "Variable attributes like references '&' e.t.c.")

;; Set font lock options.
;; For information on the various faces check here:
;; http://www.gnu.org/software/emacs/manual/html_node/ccmode/Faces.html
;; For examples on how to make advanced fontification based on the
;; language rules check C mode here:
;; http://cc-mode.sourceforge.net/src/cc-fonts.el
;;
;; Guide for Searh based fontification:
;; http://ergoemacs.org/emacs_manual/elisp/Search_002dbased-Fontification.html
;; General colouring guide:
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html
(defconst solidity-font-lock-keywords
  (list
   '(solidity-match-functions (1 font-lock-type-face)
                              (2 font-lock-function-name-face))
   '(solidity-match-contract-decl (1 font-lock-keyword-face)
                                  (2 font-lock-variable-name-face))
   '(solidity-match-variable-decls (1 font-lock-keyword-face)
                                   (2 font-lock-variable-name-face))
   `(,(regexp-opt solidity-constants 'words) . font-lock-constant-face)
   `(,(regexp-opt solidity-builtin-types 'words) . font-lock-builtin-face)
   `(,(regexp-opt solidity-keywords 'words) . font-lock-keyword-face))
  "The font lock options for solidity.")

(defun solidity-match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))

(defun solidity-match-contract-decl (limit)
  "Search the buffer forward until LIMIT matching contract declarations.

First match should be a keyword and second an identifier."
  (solidity-match-regexp
   (concat
    " *\\(contract\\) *\\(" solidity-identifier-regexp "\\)")
   limit))


(defun solidity-match-functions (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(function\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-variable-decls (limit)
  "Search the buffer forward until LIMIT matching variable declarations.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(" (regexp-opt solidity-builtin-types) "\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

;; solidity syntax table
(defvar solidity-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; '_' underscore is a valid part of a word
    (modify-syntax-entry ?_ "w" st)
    ;; c++ style comments in the syntax table
    ;; more info on the syntax flags here:
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for the solidity language.")

;; (defun solidity-mode ()
(define-derived-mode solidity-mode c-mode "solidity"
  "Major mode for editing solidity language buffers."
  (set-syntax-table solidity-mode-syntax-table)
  (use-local-map solidity-mode-map)
  ;; specify syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '(solidity-font-lock-keywords))
  ;; register indentation functions, basically the c-mode ones
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'adaptive-fill-mode)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-handle-comment)

  ;; now set their values
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'c-indent-line)
  (set (make-local-variable 'indent-region-function) 'c-indent-region)
  (set (make-local-variable 'normal-auto-fill-function) 'c-do-auto-fill)
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-line-break-function)
       'c-indent-new-comment-line)


  ;; set major mode name and run hooks
  (setq major-mode 'solidity-mode)
  (setq mode-name "solidity")
  (run-hooks 'solidity-mode-hook))

(provide 'solidity-mode)
;;; solidity-mode ends here
