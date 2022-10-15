;;; solidity-flycheck.el --- Flycheck integration for solidity emacs mode

;; Copyright (C) 2015-2018  Lefteris Karapetsas

;; Author: Lefteris Karapetsas  <lefteris@refu.co>
;; Keywords: languages, solidity, flycheck
;; Version: 0.1.11
;; Package-Requires: ((flycheck "32-cvs") (solidity-mode "0.1.9") (dash "2.17.0"))

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
(require 'dash)
(require 'project)

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

(flycheck-def-option-var flycheck-solidity-solium-soliumrcfile nil solium-check
  "The path to use for soliumrc.json

The value of this variable is either a string denoting a path to the soliumrc.json
or nil, to use the current directory.  When non-nil,
we pass the directory to solium via the `--config' option."
  :type '(choice (const :tag "No custom soliumrc" nil)
                 (string :tag "Custom soliumrc file location"))
  :safe #'stringp
  :package-version '(solidity-mode . "0.1.4"))

(defcustom solidity-flycheck-solc-additional-allow-paths nil
  "A list of paths that should be passed to solc using the --allow-paths option.

This lets you import .sol files from other directories without causing linting errors.
For example, say that you use Brownie (URL `https://github.com/eth-brownie/brownie'),
which stores ethPM packages in \"~/.brownie/packages\". You could add \"~/.brownie/packages\"
to this variable to import ethPM packages without linting errors. Subdirectories
in each allow path are remapped.

Note that when `solidity-flycheck-use-project' is t, the project root will be
added to --allow-paths in addition to any paths defined here."
  :group 'solidity
  :type 'list
  :safe #'listp
  :package-version '(solidity-mode . "0.1.11"))

(defcustom solidity-flycheck-use-project nil
  "A boolean flag denoting whether solidity-flycheck should detect projects.

When t, solidity-flycheck will detect a project and include files in the project
during compilation and linting. solidity-flycheck will look for a .soliumrc.json
in a parent directory. If found, this directory is considered the project root. If
no .soliumrc.json is found, `project-roots' is used.

When using solium-checker, the .soliumrc.json found in the project root will be
used as the solium config, rather than a .soliumrc.json in the same directory as
the file being linted.

When using solhint-checker, solhint will be run at the project root.

When using solidity-checker , the project root will be passed to solc using the
--allow-paths flag. This means imports to other files inside the project will
lint without erorr."
  :group 'solidity
  :type 'boolean
  :safe #'booleanp
  :package-version '(solidity-mode . "0.1.11"))

(defun solidity-flycheck--find-working-directory (_checker)
  "Look for a working directtory to run solium CHECKER in.

This will be a parent directory that contains a `.soliumrc.json' file. If
no .soliumrc.json is found, `project-roots' is used."
  (when (and buffer-file-name solidity-flycheck-use-project)
    (when-let ((root (or
                      (locate-dominating-file buffer-file-name ".soliumrc.json")
                      (let* ((proj (project-current t))
                             (roots (project-roots proj)))
                        (car roots)))))
      (expand-file-name root))))

  ;; define solium flycheck syntax checker
  ;; expanded the flycheck-define-checker macro in order to eval certain args, as per advice given in gitter
  ;; https://gitter.im/flycheck/flycheck?at=5a43b3a8232e79134d98872b
  ;; first try to add solium to the checker's list since if we got solc
  ;; it must come after it in the list due to it being chained after solc
(flycheck-def-executable-var solium-checker "solium")

(defun solium-has-reporter ()
  (let ((solium-full-path (funcall
                           flycheck-executable-find
                           (or flycheck-solium-checker-executable solidity-solium-path))))
    (and solium-full-path
         (string-match-p "--reporter" (shell-command-to-string (concat solium-full-path " --help"))))))

(flycheck-define-command-checker 'solium-checker
  "A Solidity linter using solium"
  :command `(,solidity-solium-path
             (eval (when (solium-has-reporter) "--reporter=gcc"))
             (option "--config=" flycheck-solidity-solium-soliumrcfile concat)
             "-f"
             source-inplace)
:error-patterns `((error line-start (zero-or-more not-newline) "[Fatal error]" (message))
                  (error line-start (zero-or-more " ") line ":" column (zero-or-more " ") "error" (message))
                  (warning line-start (zero-or-more " ") line ":" column (zero-or-more " ") "warning" (message))
                  ;; reporter=gcc formats
                  (error line-start (file-name) ":" line ":" column ": error: " (message))
                  (warning line-start (file-name) ":" line ":" column ": warning: " (message)))

  :error-filter
  ;; Add fake line numbers if they are missing in the lint output
  #'(lambda (errors)
      (dolist (err errors)
        (unless (flycheck-error-line err)
          (setf (flycheck-error-line err) 1)))
      errors)
  :modes 'solidity-mode
  :predicate #'(lambda nil (eq major-mode 'solidity-mode))
  :next-checkers `((,solidity-flycheck-chaining-error-level . solhint-checker))
  :standard-input 'nil
  :working-directory 'solidity-flycheck--find-working-directory)

;; add a solidity mode callback to set the executable of solc for flycheck
;; define solidity's flycheck syntax checker
;; expanded the flycheck-define-checker macro in order to eval certain args, as per advice given in gitter
;; https://gitter.im/flycheck/flycheck?at=5a43b3a8232e79134d98872b
(flycheck-def-executable-var solidity-checker "solc")

(defun get-solc-version ()
  "Query solc executable and return its version.

  The result is returned in a list with 3 elements.MAJOR MINOR PATCH.

 If the solc output can't be parsed an error is returned."
  (let* ((solc-full-path (funcall
                          flycheck-executable-find
                          (or flycheck-solidity-checker-executable solidity-solc-path)))
         (output (shell-command-to-string (format "%s --version" solc-full-path))))
    (if (string-match "Version: \\([[:digit:]]+\\)\.\\([[:digit:]]+\\)\.\\([[:digit:]]+\\)" output)
        (list (match-string 1 output)
              (match-string 2 output)
              (match-string 3 output))
      (error "Could not parse the output of %s --version:\n %s" solc-full-path output))))

(defun solc-gt-0.6.0 ()
  "Return `t` if solc >= 0.6.0 and `nil` otherwise."
  (let* ((version (get-solc-version))
         (major (string-to-number (nth 0 version)))
         (minor (string-to-number (nth 1 version)))
         (patch (string-to-number (nth 2 version))))
    (if (and (>= major 0) (>= minor 6)) t nil)))

(defun solidity-flycheck--solc-allow-paths ()
  (append
   (mapcar #'expand-file-name solidity-flycheck-solc-additional-allow-paths)
   '(".")))

(defun solidity-flycheck--only-subdirectories (dir)
  (-filter
   (lambda (p)
     (and (file-directory-p p)
          (not (string-prefix-p "." (file-name-nondirectory p)))))
   (directory-files dir t)))

(defun solidity-flycheck--solc-remappings ()
  (let* ((allow-paths (solidity-flycheck--solc-allow-paths)))
    (->> allow-paths
         (remove-if-not #'file-exists-p)
         (mapcar #'solidity-flycheck--only-subdirectories)
         -flatten
         (mapcar
          (lambda (dir)
            (let ((prefix (file-name-nondirectory dir)))
              (concat prefix "=" dir)))))))

(defun solidity-flycheck--solc-remappings-opt ()
  (when-let ((remappings (solidity-flycheck--solc-remappings)))
    remappings))

(defun solidity-flycheck--solc-allow-paths-opt ()
  (let ((allow-paths (mapconcat 'identity (solidity-flycheck--solc-allow-paths) ",")))
    (when (not (string= "" allow-paths))
      `("--allow-paths" ,allow-paths))))

(defun solidity-flycheck--solc-cmd ()
  `(,solidity-solc-path
    (eval
     (when (solc-gt-0.6.0)
       `("--no-color"
         ,@(solidity-flycheck--solc-allow-paths-opt)
         ,@(solidity-flycheck--solc-remappings-opt))))
    source-inplace))

(flycheck-define-command-checker 'solidity-checker
  "A Solidity syntax checker using the solc compiler"
  :command (solidity-flycheck--solc-cmd)
  :error-patterns '(
                    ;; Solidity >= 0.6.0 error formats
                    (error line-start "Error: " (message) "\n" (zero-or-more whitespace) "--> " (file-name) ":" line ":" column)
                    (warning line-start "Warning: " (message) "\n" (zero-or-more whitespace) "--> " (file-name) ":" line ":" column)

                    ;; Solidity < 0.6.0 error formats
                    (error line-start (file-name) ":" line ":" column ":" " Error: " (message))
                    (error line-start (file-name) ":" line ":" column ":" " Compiler error: " (message))
                    (error line-start "Error: " (message))
                    (warning line-start (file-name) ":" line ":" column ":" " Warning: " (message)))
  :modes 'solidity-mode
  :predicate #'(lambda nil (eq major-mode 'solidity-mode))
  :next-checkers
  `((,solidity-flycheck-chaining-error-level . solium-checker)
    (,solidity-flycheck-chaining-error-level . solhint-checker))
  :standard-input 'nil
  :working-directory 'solidity-flycheck--find-working-directory)

(flycheck-def-executable-var solhint-checker "solhint")
(flycheck-define-command-checker 'solhint-checker
  "A Solidity linter using solhint"
  :command `(,solidity-solhint-path "-f" "unix" source-inplace)
  :error-patterns `((error
                     line-start (file-name) "("  line "," column "):" (zero-or-more " ")
                     "error" (zero-or-more " ") (message))
                    (warning
                     line-start (file-name) "("  line "," column "):" (zero-or-more " ")
                     "warning" (zero-or-more " ") (message)))
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
  :working-directory 'solidity-flycheck--find-working-directory)

(add-to-list 'flycheck-checkers 'solhint-checker)
(add-to-list 'flycheck-checkers 'solium-checker)
(add-to-list 'flycheck-checkers 'solidity-checker)

(provide 'solidity-flycheck)
;;; solidity-flycheck.el ends here
