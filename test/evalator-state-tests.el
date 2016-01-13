(require 'ert)
(require 'evalator-state)

(ert-deftest evalator-state-init-test ()
  "Tests that 'evalator state' is initialized with the value of
'evalator-state-default'"
  (let ((state-default-copy (copy-sequence evalator-state-default))
        (state-copy (copy-sequence evalator-state)))
    (unwind-protect
        (progn
          (setq evalator-state-default '(:foo))
          (evalator-state-init)
          (should (equal '(:foo) (evalator-state-init))))
      (setq evalator-state-default state-default-copy)
      (setq evalator-state state-copy))))



