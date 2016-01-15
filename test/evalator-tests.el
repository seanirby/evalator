(require 'ert)
(require 'evalator)

(ert-deftest evalator-action-previous-and-next-tests ()
  (let ((evalator-state (list :history [(:candidates '())
                                        (:candidates '())]
                              :history-index 0)))
    (flet ((helm-set-pattern (_) nil)
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
    (flet ((slot-value (_o _s) nil)
           (helm-set-pattern (p) (setq helm-pattern p)))

      ;;When transform-candidates returns non-nil the candidates are pushed on history
      (flet ((evalator-transform-candidates (&rest _) '("foo")))
        (evalator-action-confirm)
        (should (equal (list :candidates '("foo")
                             :expression "bar")
                       (evalator-history-current))))
      
      ;;Otherwise nothing is done
      (flet ((evalator-transform-candidates (&rest args) nil))
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

(ert-deftest evalator-transform-candidates-test ()
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
                       (evalator-transform-candidates nil nil make-f transform-f)))

        (should (equal :success flash-status))

        (should (equal '("foo")
                       (evalator-transform-candidates "(list 1" nil make-f transform-f)))

        (should (equal :error flash-status))

        (setq evalator-state (plist-put evalator-state :history-index 1))
        
        ;; transform-f called otherwise
        (should (equal "transform-f-called"
                       (evalator-transform-candidates nil nil make-f transform-f)))

        (should (equal :success flash-status))

        (should (equal '("bar")
                       (evalator-transform-candidates "(list 1" nil make-f transform-f)))

        (should (equal :error flash-status))))))

(ert-deftest evalator-insert-equiv-expr-test ()
  (let ((expr-chain '("(list 1 2 3)" "(reduce '+ Ⓔ)" "(+ Ⓔ 1)")))
    (with-temp-buffer
      (evalator-insert-equiv-expr expr-chain)
      (should (equal "(+ (reduce '+ (list 1 2 3)) 1)"
                     (buffer-string))))))

(ert-deftest evalator-init-history ()
  (flet ((evalator-history-push! (source expr) t))
    (should (equal t (evalator-init-history nil nil t)))
    (should (equal nil (evalator-init-history nil nil nil)))))

(ert-deftest evalator-init-tests ()
  (let ((init-f (lambda () t))
        (evalator-state '(:initialized nil :mode nil)))
    (flet ((evalator-state-init ()
                                (setq evalator-state
                                      (plist-put evalator-state :initialized t)))
           (evalator-utilts-put! (state k v)
                                 (setq state
                                       (plist-put state k v))))
      "init-f should not be called"
      (should (equal nil (evalator-init init-f nil :dummy-mode)))
      "evalator-state should not be initialized"
      (should (equal nil (plist-get evalator-state :initialized)))
      "mode should not be set"
      (should (equal nil (plist-get evalator-state :mode)))
      
      "init-f should be called"
      (should (equal t (evalator-init init-f t :dummy-mode)))
      "mode should be set"
      (should (equal :dummy-mode (plist-get evalator-state :mode)))
      "evalator-state should be initialized"
      (should (equal t (plist-get evalator-state :initialized))))))

(ert-deftest evalator-test ()
  (let ((evalator-candidates-initial '("foo")))
    (flet ((evalator-build-source (candidates _) candidates)
           (helm (&rest args) (cadr args)))
      (should (equal '("foo") (evalator)))
      (should (equal '("bar") (evalator :candidates '("bar")))))))

(ert-deftest evalator-explicit-test ()
  (flet ((evalator (&rest args) (plist-get args :mode)))
    (should (equal :explicit (evalator-explicit)))))
