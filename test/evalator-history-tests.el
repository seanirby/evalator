(require 'evalator-history)

(ert-deftest evalator-history-test ()
  "Test that 'evalator-history' returns the ':history' value of
'evalator-state'"
  (let ((state-copy (copy-sequence evalator-state)))
    (unwind-protect
        (progn (setq evalator-state '(:history :foo))
               (should (equal :foo (evalator-history))))
      (setq evalator-state state-copy))))

(ert-deftest evalator-history-index-test ()
  "Tests that 'evalator-history-index' returns the ':history-index'
property of 'evalator-state'"
  (let ((evalator-state '(:history-index 0)))
    (should (equal 0 (evalator-history-index)))))

(ert-deftest evalator-history-push!-test ()
  "Tests that 'evalator-history-push!' can push onto state's history
and update state's index."
  (let ((evalator-state '(:history [] :history-index -1)))
    (progn
      (evalator-history-push! '("foo") "bar")
      (should (equal
               '(:history [(:source ("foo") :expression "bar")] :history-index 0) 
               evalator-state)))))

(ert-deftest evalator-history-current-test ()
  (flet ((evalator-history () ["foo" "bar" "baz"])
         (evalator-history-index () 1))
    (should (equal "bar"
                   (evalator-history-current)))))
