(require 'ert)
(require 'evalator-state)

(ert-deftest evalator-state-init-test ()
  "Tests that 'evalator state' is initialized with the value of
'evalator-state-default'"
  (let ((evalator-state-default (copy-sequence evalator-state-default))
        (evalator-state nil))
    (progn
      (evalator-state-init :normal)
      (should (equal evalator-state-default evalator-state))

      (evalator-state-init :explicit)
      (should-not (equal evalator-state-default evalator-state))
      (should (equal :explicit (plist-get evalator-state :mode))))))



