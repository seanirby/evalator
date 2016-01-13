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

;; TODO Not sure how to test this one yet
(ert-deftest evalator-flash-test ()
  "Tests that 'evalator-flash' changes the 'minibuffer-prompt' face"
  )

;; TODO Not sure how to test this one yet
(ert-deftest evalator-marked-candidates ()
  "Tests that 'evalator-marked-candidates' returns marked candidates
or nil"
  )

(ert-deftest evalator-persistent-help-test ()
  "Tests that 'evalator-persistent-help' builds persistent help
string."
  (let ((evalator-key-map (list 'evalator-action-previous   "C-l"
                                'evalator-action-confirm    "RET"
                                'evalator-action-insert-arg "C-;")))
    (flet ((where-is-internal (command key-map _) (plist-get key-map command))
           (key-description (str) str))
      (should (equal (concat "History forward, "
                             "C-l: History backward, "
                             "RET: Accept transformation, "
                             "C-;: Insert special arg")
                     (evalator-persistent-help))))))

;; Tried to mock the helm-build-sync-source macro but ran into issues
;; This works for now...
(ert-deftest evalator-build-source-test ()
  "Tests that name of source is set correctly depending on the 'mode'
argument passed to 'evalator-build-source'"
  (let ((args-normal (evalator-build-source nil :normal))
        (args-explicit (evalator-build-source nil :explicit)))
    (should (equal "Evaluation Result"
                   (cdr (assoc 'name args-normal))))
    (should (equal "Evaluation Result(Explicit)"
                   (cdr (assoc 'name args-explicit))))))

(ert-deftest evalator-transform-candidates-test ()
  ""
  (let ((flash-status nil))
    (flet ((helm-get-current-source () nil)
           (helm-get-candidates (_) '("foo" "bar"))
           (evalator-marked-candidates () nil)
           (evalator-flash (status) (setq flash-status status)))
      (let ((make-f (lambda (bad-expr _mode)
                      (if bad-expr
                          (eval (read bad-expr))
                        "make-f-called")))
            (transform-f (lambda (_cands-all _cands-marked bad-expr _mode)
                           (if bad-expr
                               (eval (read bad-expr))
                             "transform-f-called"))))
        (flet ((evalator-history-index () 0))
          ;; make-f is called at index 0
          (should (equal "make-f-called"
                         (evalator-transform-candidates nil nil make-f transform-f)))
          ;; Flash status is ':success' call succeeds
          (should (equal :success flash-status))
          ;; All candidates get returned on error
          (should (equal '("foo" "bar")
                         (evalator-transform-candidates "(list 1" nil make-f transform-f)))
          ;; Flash status is ':error' when call fails
          (should (equal :error flash-status)))
        (flet ((evalator-history-index () 1))
          ;; transform-f called otherwise
          (should (equal "transform-f-called"
                         (evalator-transform-candidates nil nil make-f transform-f)))
          ;; Flash status is ':success' call succeeds
          (should (equal :success flash-status))
          ;; All candidates get returned on error
          (should (equal '("foo" "bar")
                         (evalator-transform-candidates "(list 1" nil make-f transform-f)))
          ;; Flash status is ':error' when call fails
          (should (equal :error flash-status)))))))
