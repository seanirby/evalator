(require 'ert)
(require 'evalator)

(ert-deftest evalator-action-previous-and-next-tests ()
  (let ((evalator-state (list :history [(:candidates '())
                                        (:candidates '())]
                              :history-index 0)))
    (flet ((evalator-unmark-all () nil)
           (helm-set-pattern (_) nil)
           (helm-update () nil))
      
      (evalator-action-previous)
      (should (equal 0 (evalator-history-index)))

      (evalator-action-next)
      (should (equal 1   (evalator-history-index)))

      (evalator-action-next)
      (should (equal 1 (evalator-history-index)))

      (evalator-action-previous)
      (should (equal 0   (evalator-history-index))))))

(ert-deftest evalator-action-confirm-test ()
  (let ((helm-pattern "bar")
        (evalator-state (list :history []
                              :history-index -1)))
    (flet ((evalator-unmark-all () nil)
           (slot-value (_o _s) nil)
           (helm-set-pattern (p) (setq helm-pattern p)))

      ;;When transform-candidates returns non-nil the candidates are pushed on history
      (flet ((evalator-make-or-transform-candidates (&rest _) '("foo")))
        (evalator-action-confirm)
        (should (equal (list :candidates '("foo")
                             :expression "bar")
                       (evalator-history-current))))
      
      ;;Otherwise nothing is done
      (flet ((evalator-make-or-transform-candidates (&rest args) nil))
        (evalator-action-confirm)
        (should (equal (list :candidates '("foo")
                             :expression "bar")
                       (evalator-history-current)))))))

(ert-deftest evalator-action-insert-special-arg-test ()
  (let ((evalator-context-special-arg-default "Ⓔ"))
    (should (equal "Ⓔ"
                   (with-temp-buffer
                     (evalator-action-insert-special-arg)
                     (buffer-string))))))

;; TODO
(ert-deftest evalator-flash-test ())

;; TODO
(ert-deftest evalator-unmark-all ())

;; TODO
(ert-deftest evalator-marked-candidates ())

(ert-deftest evalator-mpersistent-help-test ()
  (let ((evalator-key-map (list 'evalator-action-previous           "C-l"
                                'evalator-action-confirm            "RET"
                                'evalator-action-insert-special-arg "C-;")))
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
  (let ((args-normal (evalator-build-source nil :normal))
        (args-explicit (evalator-build-source nil :explicit)))
    (should (equal "Evaluation Result"
                   (cdr (assoc 'name args-normal))))
    (should (equal "Evaluation Result(Explicit)"
                   (cdr (assoc 'name args-explicit))))))

(ert-deftest evalator-make-or-transform-candidates-test ()
  (let ((evalator-state (list :history [(:candidates ("foo"))
                                        (:candidates ("bar"))]
                              :history-index 0))
        (flash-status nil))
    (flet ((evalator-marked-candidates () nil)
           (evalator-flash (status) (setq flash-status status)))
      (let ((make-f (lambda (bad-expr _mode)
                      (if bad-expr
                          (eval (read bad-expr))
                        "make-f-called")))
            (transform-f (lambda (_cands-all _cands-marked bad-expr _mode)
                           (if bad-expr
                               (eval (read bad-expr))
                             "transform-f-called"))))
        ;; make-f is called at index 0
        (should (equal "make-f-called"
                       (evalator-make-or-transform-candidates nil nil make-f transform-f)))

        (should (equal :success flash-status))

        (should (equal '("foo")
                       (evalator-make-or-transform-candidates "(list 1" nil make-f transform-f)))

        (should (equal :error flash-status))

        (setq evalator-state (plist-put evalator-state :history-index 1))
        
        ;; transform-f called otherwise
        (should (equal "transform-f-called"
                       (evalator-make-or-transform-candidates nil nil make-f transform-f)))

        (should (equal :success flash-status))

        (should (equal '("bar")
                       (evalator-make-or-transform-candidates "(list 1" nil make-f transform-f)))

        (should (equal :error flash-status))))))

(ert-deftest evalator-insert-equiv-expr-test ()
  (let ((expr-chain '("(list 1 2 3)" "(reduce '+ Ⓔ)" "(+ Ⓔ 1)")))
    (with-temp-buffer
      (evalator-insert-equiv-expr expr-chain)
      (should (equal "(+ (reduce '+ (list 1 2 3)) 1)"
                     (buffer-string))))))

(ert-deftest evalator-test ()
  (let ((state-init-p nil)
        (history nil)
        (evalator-candidates-initial '("foo")))
    (flet ((evalator-state-init (_)
                                (setq state-init-p t))
           (evalator-history-push! (cands expr)
                                   (setq history (list :candidates cands
                                                       :expression expr)))
           (evalator-build-source (cands mode) `(,cands ,mode))
           (helm (&rest args) (cadr args)))

      ;;helm should be called with the result from evalator-build-source as the :source
      (should (equal '(("foo") :explicit)
                     (evalator :explicit)))

      (should state-init-p)

      (should (equal (list :candidates '("foo")
                           :expression "")
                     history)))))
