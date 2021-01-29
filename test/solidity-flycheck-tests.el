;;; solidity-flycheck-tests.el --- Setup and execute all tests

;;; Commentary:

;; This package contains tests for the solidity-flycheck package.

;;; Code:

(require 'solidity-flycheck)

(ert-deftest test-solidity-flycheck--solc-allow-paths ()
  "Test that `solidity-flycheck--solc-allow-paths' contains `solidity-flycheck-solc-additional-allow-paths'."
  (let ((solidity-flycheck-solc-additional-allow-paths '("/tmp/test1" "/tmp/test2")))
    (should (equal
             (solidity-flycheck--solc-allow-paths)
             '("/tmp/test1" "/tmp/test2" ".")))))

(defmacro with-temp-dirs (temp-dirs &rest body)
  (let ((bindings (mapcar (lambda (d) `(,d (make-temp-file "" t)))
                          temp-dirs)))
    `(let ,bindings
       (unwind-protect
           (progn
             ,@body)
         (progn
           (dolist (d (list ,@temp-dirs))
             (delete-directory d t)))))))

(ert-deftest test-solidity-flycheck--solc-remappings ()
  "Test that `solidity-flycheck--solc-remappings' creates remappings for path in `solidity-flycheck--solc-allow-paths'"
  (with-temp-dirs
   (test-dir-1 test-dir-2 test-dir-3)
   (let* ((default-directory test-dir-1)
          (solidity-flycheck-solc-additional-allow-paths (list test-dir-2 test-dir-3)))
     (make-directory (concat (file-name-as-directory test-dir-2) "subdir1"))
     (make-directory (concat (file-name-as-directory test-dir-3) "@subdir2"))
     (should (equal
              (solidity-flycheck--solc-remappings)
              (list (concat "subdir1=" (concat (file-name-as-directory test-dir-2) "subdir1"))
                    (concat "@subdir2=" (concat (file-name-as-directory test-dir-3) "@subdir2")))))))
  )

(provide 'solidity-flycheck-tests)
;;; solidity-flycheck-tests.el ends here
