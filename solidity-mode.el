;;; solmode.el --- Major Emacs mode for ethereum solidity
;;; Commentary:
;;  Followed the Emacs mode tutorial to make it
;;  http://www.emacswiki.org/emacs/ModeTutorial
;;; Code:

(defvar solidity-mode-hook nil)

(defvar solidity-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for solidity major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

(defconst solidity-keywords
  '("implof" "defimplof" "fn" "module" "signature" "import" "for" "in"
    "if" "else" "elif" "return")
  "Keywords of the solidity language.")

(defconst solidity-constants
  '("true" "false" "nil")
  "Constants in the solidity language.")

(defconst solidity-builtin-types
  '("i8" "u8"
    "i16" "u16"
    "i32" "u32"
    "i64" "u64"
    "f32" "f64"
    "string" "string8"
    "bool")
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
   '(refu-match-data-decl (1 font-lock-keyword-face)
                          (2 font-lock-variable-name-face))
   `(,(regexp-opt solidity-constants 'words) . font-lock-constant-face)
   `(,(regexp-opt solidity-builtin-types 'words) . font-lock-builtin-face)
   '(solidity-match-types-after-functions 2 font-lock-type-face)
   '(solidity-match-functions 1 font-lock-function-name-face)
   ;; '(refu-match-module-impl-name 2 font-lock-constant-face)
   ;; '(refu-match-module-signature-name 1 font-lock-constant-face)
   ;; '(refu-match-variable-decl-gen (1 font-lock-variable-name-face)
   ;;                                (3 font-lock-type-face)
   ;;                                (4 font-lock-type-face))
   ;; '(refu-match-variable-decl (1 font-lock-variable-name-face)
   ;;                            (2 font-lock-type-face))
   `(,(regexp-opt solidity-keywords 'words) . font-lock-keyword-face)
   )
  "The font lock options for solidity.")

(defun solidity-match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))


(defun solidity-match-functions (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
      "function *\\(" solidity-identifier-regexp "\\)")
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

(defun solidity-indent-line ()
  "Indent a line as solidity code."
  (interactive)
  (beginning-of-line) ()
  (if (bobp)            ; if at beginning of buffer
      (indent-line-to 0)
    ;else
    (progn)))

(defun solidity-mode ()
  "Major mode for editing solidity language buffers."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table solidity-mode-syntax-table)
  (use-local-map solidity-mode-map)
  ;; specify syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '(solidity-font-lock-keywords))
  ;; register indentation functions
  (set (make-local-variable 'indent-line-function) 'solidity-indent-line)
  ;; set major mode name and run hooks
  (setq major-mode 'refu-mode)
  (setq mode-name "refu")
  (run-hooks 'refu-mode-hook))

(provide 'solidity-mode)
;;; solidity-mode ends here
