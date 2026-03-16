;;; xq-overrides-test.el --- Tests for override helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'xq-overrides)

(ert-deftest xq/disable-module-removes-feature ()
  (let ((xq/modules '(xq-ui xq-org xq-keybinds)))
    (xq/disable-module 'xq-org)
    (should (equal xq/modules '(xq-ui xq-keybinds)))))

(ert-deftest xq/enable-module-inserts-after-reference ()
  (let ((xq/modules '(xq-ui xq-terminal xq-keybinds)))
    (xq/enable-module 'xq-org 'xq-terminal)
    (should (equal xq/modules '(xq-ui xq-terminal xq-org xq-keybinds)))))

(ert-deftest xq/enable-module-does-not-duplicate-feature ()
  (let ((xq/modules '(xq-ui xq-org xq-keybinds)))
    (xq/enable-module 'xq-org 'xq-ui)
    (should (equal xq/modules '(xq-ui xq-org xq-keybinds)))))

(provide 'xq-overrides-test)
;;; xq-overrides-test.el ends here
