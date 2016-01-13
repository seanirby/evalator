(require 'ert)
(require 'evalator)

(ert-deftest evalator-load-test ()
  "Tests that 'helm-exit-and-execute-action' is called with a callback
to start a new evalator session whose candidates are the candidates in
the current source in history."
  (flet ((evalator-history-current (_) '(:candidates ("foo")))
         (helm-get-candidates (source) (plist-get source :candidates))
         (helm-exit-and-execute-action (f) (funcall f nil))
         (evalator (&rest args) args))
    (should (equal '(:candidates ("foo") :initp nil :hist-pushp nil)
                   (evalator-load)))))

(ert-deftest evalator-action-previous-and-next-tests ()
  "Tests that 'evalator-action-(previous | next) move loads previous
or next item in history, respectively."
  (let ((evalator-state '(:history [0 1] :history-index 0)))
    (flet ((evalator-load ()
                          (elt (plist-get evalator-state :history)
                               (plist-get evalator-state :history-index))))
      (should (equal nil (evalator-action-previous)))
      (should (equal 1   (evalator-action-next)))
      (should (equal nil (evalator-action-next)))
      (should (equal 0   (evalator-action-previous))))))

(ert-deftest evalator-action-confirm-test ()
  "Tests tha successful confirmation calls evalator with the correct
arguments.  Tests that a failing confirmation returns nil and outputs
an error message."
  (flet ((evalator (&rest args) args)
         (helm-exit-and-execute-action (f) (funcall f nil)))
    (flet ((evalator-transform-candidates (&rest _) '("foo" "bar" "baz")))
      (should (equal (list :candidates '("foo" "bar" "baz")
                           :initp      nil
                           :hist-pushp t)
                     (evalator-action-confirm))))
    (flet ((message (_)
                    (insert "Dummy Error Message"))
           ;; Call error handler
           (evalator-transform-candidates (&rest args) (funcall (car (last args)))))
      (with-temp-buffer
        (should (equal nil (evalator-action-confirm)))
        (should (equal "Dummy Error Message" (buffer-string)))))))

(ert-deftest evalator-action-insert-arg-test ()
  "Tests that the special arg character is inserted into a buffer"
  (let ((evalator-context-special-arg-default "*"))
    (should (equal "*"
                   (with-temp-buffer
                     (evalator-action-insert-arg)
                     (buffer-string))))))
