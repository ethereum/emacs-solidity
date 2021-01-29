;;; solidity-mode-test-setup.el --- Setup and execute all tests

;;; Commentary:

;; This package sets up a suitable enviroment for testing
;; solidity-mode, and executes the tests.
;;
;; Usage:
;;
;;   emacs -q -l test/solidity-mode-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.

;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(add-to-list 'load-path (expand-file-name "./"))
(add-to-list 'load-path (expand-file-name "./test"))
(package-install-file "solidity-mode.el")
(package-install-file "solidity-flycheck.el")
(global-flycheck-mode 1)

(use-package solidity-mode
  :mode ("\\.sol\\'" . solidity-mode))

(use-package solidity-flycheck
  :defer t
  :init
  (setq solidity-flycheck-solium-checker-active t)
  (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-chaining-error-level t)
  (setq solidity-flycheck-use-project t)
  (setq solidity-flycheck-solc-additional-allow-paths '("~/.brownie/packages"))
  (add-hook
   'solidity-mode-hook
   (lambda ()
     (require 'solidity-flycheck))))

(require 'ert)

(require 'solidity-flycheck-tests)

(ert t)
