;;; solidity-common.el --- Common code used by all sourcefiles

;; Copyright (C) 2015-2018  Lefteris Karapetsas

;; Author: Lefteris Karapetsas  <lefteris@refu.co>
;; Keywords: languages

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

;; Some common definitions used by all source files of the package are defined
;; here in order to avoid circular dependencies
;;
;;; Code:

(defcustom solidity-solc-path "solc"
  "Path to the solc binary."
  :group 'solidity
  :type 'string
  :package-version '(solidity . "0.1.1"))

(defcustom solidity-solium-path "solium"
  "Path to the solium binary."
  :group 'solidity
  :type 'string
  :package-version '(solidity . "0.1.4"))

(provide 'solidity-common)
;;; solidity-common.el ends here
