;;; run-tests.el --- Test entry point -*- lexical-binding: t; -*-

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory))
  (add-to-list 'load-path (expand-file-name "../lisp" test-directory)))

(require 'ert)
(require 'xq-overrides-test)

;;; run-tests.el ends here
