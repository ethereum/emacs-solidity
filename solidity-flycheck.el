;;; solidity-flycheck.el --- Flycheck integration for solidity emacs mode

;; Copyright (C) 2015-2018  Lefteris Karapetsas

;; Author: Lefteris Karapetsas  <lefteris@refu.co>
;; Keywords: languages, solidity, flycheck
;; Version: 0.1.10
;; Package-Requires: ((flycheck "32-cvs") (solidity-mode "0.1.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; flycheck integration is taken in a different file to prevent problems
;; when flycheck is not present on the user's system.
;;
;;; Code:

(require 'flycheck)
(require 'solidity-common)

(defvar flycheck-solidity-checker-executable)
(defvar flycheck-solium-checker-executable)

(defcustom solidity-flycheck-solc-checker-active nil
  "A boolean flag denoting if solc flycheck checker should be active."
  :group 'solidity
  :type 'boolean
  :safe #'booleanp
  :package-version '(solidity . "0.1.5"))

(defcustom solidity-flycheck-chaining-error-level 'warning
  "The maximum error level at which chaining of checkers will happen.

This means that this is the error level for which solc checker will allow
next checkers to run.  By default this is the warning level.
Possible values are:

`info'
    If any errors higher than info level are found in solc, then solium
    will not run.

`warning'
    If any errors higher than warning level are found in solc, then solium
    will not run.

`error'
    If any errors higher than error level are found in solc, then solium
    will not run.
 t
    Solium will always run."
  :group 'solidity
  :type '(choice (const :tag "Chain after info-error level" info)
                 (const :tag "Chain after warning-error level" warning)
                 (const :tag "Chain after error-error level" error)
                 (const :tag "Always chain" t))
  :package-version '(solidity . "0.1.5")
  :safe #'symbolp)

(defcustom solidity-flycheck-solium-checker-active nil
  "A boolean flag denoting if solium flycheck checker should be active."
  :group 'solidity
  :type 'boolean
  :safe #'booleanp
  :package-version '(solidity . "0.1.5"))

(flycheck-def-option-var flycheck-solidity-solium-soliumrcfile nil solium-check
  "The path to use for soliumrc.json

The value of this variable is either a string denoting a path to the soliumrc.json
or nil, to use the current directory.  When non-nil,
we pass the directory to solium via the `--config' option."
  :type '(choice (const :tag "No custom soliumrc" nil)
                 (string :tag "Custom soliumrc file location"))
  :safe #'stringp
  :package-version '(solidity-mode . "0.1.4"))

(when solidity-flycheck-solium-checker-active
  ;; define solium flycheck syntax checker
  ;; expanded the flycheck-define-checker macro in order to eval certain args, as per advice given in gitter
  ;; https://gitter.im/flycheck/flycheck?at=5a43b3a8232e79134d98872b
  ;; first try to add solium to the checker's list since if we got solc
  ;; it must come after it in the list due to it being chained after solc
  (flycheck-def-executable-var solium-checker "solium")
  (let ((solium-full-path (funcall flycheck-executable-find solidity-solium-path)))
    (if solium-full-path
        (let ((solium-has-reporter (string-match-p "--reporter" (shell-command-to-string (concat solium-full-path " --help")))))
          (flycheck-define-command-checker 'solium-checker
            "A Solidity linter using solium"
            :command `("solium"
                       ,(if solium-has-reporter "--reporter=gcc" "")
                       (option "--config=" flycheck-solidity-solium-soliumrcfile concat)
                       "-f"
                       source-inplace)
            :error-patterns `((error line-start (zero-or-more not-newline) "[Fatal error]" (message))
                              ,(if solium-has-reporter
                                   '(error line-start (file-name) ":" line ":" column ": error: " (message))
                                 '(error line-start (zero-or-more " ") line ":" column (zero-or-more " ") "error" (message)))
                              ,(if solium-has-reporter
                                   '(warning line-start (file-name) ":" line ":" column ": warning: " (message))
                                 '(warning line-start (zero-or-more " ") line ":" column (zero-or-more " ") "warning" (message))))
            :error-filter
            ;; Add fake line numbers if they are missing in the lint output
            #'(lambda (errors)
                (dolist (err errors)
                  (unless (flycheck-error-line err)
                    (setf (flycheck-error-line err) 1)))
                errors)
            :modes 'solidity-mode
            :predicate #'(lambda nil (eq major-mode 'solidity-mode))
            :next-checkers 'nil
            :standard-input 'nil
            :working-directory 'nil)
          (add-to-list 'flycheck-checkers 'solium-checker)
          (setq flycheck-solium-checker-executable solidity-solium-path))
      (error (format "Solidity Mode Configuration error. Requested solium flycheck integration but can't find solium at: %s" solidity-solium-path)))))

(defun get-solc-version ()
  "Query solc executable and return its version.

  The result is returned in a list with 3 elements.MAJOR MINOR PATCH.

 If the solc output can't be parsed an error is returned."
  (let ((output (shell-command-to-string (format "%s --version" solidity-solc-path))))
    (if (string-match "Version: \\([[:digit:]]+\\)\.\\([[:digit:]]+\\)\.\\([[:digit:]]+\\)" output)
        (list (match-string 1 output)
              (match-string 2 output)
              (match-string 3 output))
      (error "Could not parse the output of %s --version:\n %s" solidity-solc-path output))))

(defun solc-gt-0.6.0 ()
  "Return `t` if solc >= 0.6.0 and `nil` otherwise."
  (let* ((version (get-solc-version))
         (major (string-to-number (nth 0 version)))
         (minor (string-to-number (nth 1 version)))
         (patch (string-to-number (nth 2 version))))
    (if (and (>= major 0) (>= minor 6)) t nil)))

(when solidity-flycheck-solc-checker-active
  ;; add a solidity mode callback to set the executable of solc for flycheck
  ;; define solidity's flycheck syntax checker
  ;; expanded the flycheck-define-checker macro in order to eval certain args, as per advice given in gitter
  ;; https://gitter.im/flycheck/flycheck?at=5a43b3a8232e79134d98872b
  (flycheck-def-executable-var solidity-checker "solc")
  (let* ((cmd (if (solc-gt-0.6.0) '("solc" "--old-reporter" source-inplace) '("solc" source-inplace))))
    (if (funcall flycheck-executable-find solidity-solc-path)
        (progn
          (flycheck-define-command-checker 'solidity-checker
            "A Solidity syntax checker using the solc compiler"
            :command cmd
            :error-patterns '(
                              (error line-start (file-name) ":" line ":" column ":" " Error: " (message))
                              (error line-start (file-name) ":" line ":" column ":" " Compiler error: " (message))
                              (error line-start "Error: " (message))
                              (warning line-start (file-name) ":" line ":" column ":" " Warning: " (message)))
            :modes 'solidity-mode
            :predicate #'(lambda nil (eq major-mode 'solidity-mode))
            :next-checkers `((,solidity-flycheck-chaining-error-level . solium-checker))
            :standard-input 'nil
            :working-directory 'nil)
          (add-to-list 'flycheck-checkers 'solidity-checker)
          (setq flycheck-solidity-checker-executable solidity-solc-path))
      (error (format "Solidity Mode Configuration error. Requested solc flycheck integration but can't find solc at: %s" solidity-solc-path)))))

(provide 'solidity-flycheck)
;;; solidity-flycheck.el ends here
