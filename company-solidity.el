;;; company-solidity.el --- Company-mode back-end for solidity-mode

;; Copyright (C) 2018  Samuel Smolkin

;; Author: Samuel Smolkin <sam@future-precedent.org>
;; URL: https://github.com/ethereum/emacs-solidity
;; Keywords: solidity, completion, company
;; Version: 2.0.0
;; Package-Requires: ((company "0.9.0") (cl-lib "0.5.0"))

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

;; This package provides a simple company-mode back-end for auto-completing Solidity keywords when working in solidity-mode.

;;; Code:

(require 'cl-lib)
(require 'company)

;; Additional completion targets whcih are nore part of solidty-mode syntax-highlighting keywords lists:

(defconst company-solidity-additional-math
  '("addmod"
    "mulmod"))

(defconst company-solidity-additional-hashing
  '("keccak256"
    "sha256"
    "sha3"
    "ripemd160"
    "ecrecover"))

(defconst company-solidity-additional-block-methods
  '("block.blockhash"
    "block.coinbase"
    "block.difficulty"
    "block.gaslimit"
    "block.number"
    "block.timestamp"
    "now"))

(defconst company-solidity-additional-msg-methods
  '("msg.data"
    "msg.gas"
    "msg.sender"
    "msg.sig"
    "msg.value"
    "gasleft"))

(defconst company-solidity-additional-tx-methods
  '("tx.gasprice"
    "tx.origin"))

(defconst company-solidity-additional-address-methods
  '("balance"
    "transfer"
    "send"
    "call"
    "callcode"
    "delegatecall"))

(defconst company-solidity-additional-contracts
  '("super"
    "selfdestruct"
    "suicide"))

(defconst company-solidity-additional-modifiers
  '("payable"))

(defconst company-solidity-additional-pragma
  '("solidity"))

(defconst company-solidity-additional-types
  '("fixed"
    "ufixed"
    "hex"))

(defconst company-solidity-additional-function-methods
  '("selector"))

;; defvar symbols taken from solidity-mode.el to avoid reference warnings
(defvar solidity-keywords)
(defvar solidity-constants)
(defvar solidity-variable-modifier)
(defvar solidity-builtin-types)
(defvar solidity-builtin-constructs)

;; Completion targets taken from solidity-mode syntax-highlighting keywords lists, plus additional targets above.
(defconst company-solidity-keywords
  (append
   solidity-keywords
   solidity-constants
   solidity-variable-modifier
   solidity-builtin-types
   solidity-builtin-constructs
   company-solidity-additional-math
   company-solidity-additional-hashing
   company-solidity-additional-block-methods
   company-solidity-additional-msg-methods
   company-solidity-additional-tx-methods
   company-solidity-additional-address-methods
   company-solidity-additional-contracts
   company-solidity-additional-modifiers
   company-solidity-additional-pragma
   company-solidity-additional-types
   company-solidity-additional-function-methods))

;;;###autoload
(defun company-solidity (command &optional arg &rest ignored)
  "Autocompletion for solidity with company mode.

Argument COMMAND `company-backend` functions.
Optional argument ARG the completion target prefix.
Optional argument IGNORED Additional arguments are ingnored."
    (interactive (list 'interactive))
    (set (make-local-variable 'company-minimum-prefix-length) 2)
    (cl-case command
	(interactive (company-begin-backend 'company-solidity))
	(prefix (and (eq major-mode 'solidity-mode)
		    (company-grab-symbol)))
    (candidates
    (cl-remove-if-not
	(lambda (c) (string-prefix-p arg c))
	company-solidity-keywords))))

(add-to-list 'company-backends 'company-solidity)

(provide 'company-solidity)

;;; company-solidity.el ends here
