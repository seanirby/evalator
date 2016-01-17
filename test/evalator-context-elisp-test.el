(require 'ert)
(require 'evalator-context-elisp)

(setq evalator-context-elisp-special-arg "Ⓔ")
(defalias 'ssa 'evalator-context-elisp-subst-special-args)
(defalias 'mc 'evalator-context-elisp-make-candidates)
(defalias 'tc 'evalator-context-elisp-transform-candidates)

(ert-deftest evalator-context-elisp-eval ()
  (should (equal 2
                 (evalator-context-elisp-eval "(+ 1 1)" nil))))

(ert-deftest evalator-context-elisp-subst-special-args ()
  (let ((sa evalator-context-elisp-special-arg))
    (should (equal "'(+ 1 1)"
                   (ssa "'(+ 1 1)" nil sa)))

    (should (equal "'(+ 1 1)"
                   (ssa "'(+ 1 Ⓔ)" 1 sa)))

    (should (equal "'(+ 1 1)"
                   (ssa "'(+ 1 Ⓔ0)" '(1 2 3) sa)))

    (should (equal "'(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1)))))"
                   (ssa "'(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 Ⓔ0)))))" [1 2 3] sa)))

    (should (equal "1"
                   (ssa "Ⓔ" 1 sa)))))

(ert-deftest evalator-context-elisp-make-candidates-tests ()
  ;;normal mode
  (should (equal '("1" "2" "3")
                 (mc "'(1 2 3)" :normal t)))

  (should (equal '("1" "2" "3")
                 (mc "[1 2 3]" :normal t)))

  (should (equal '("\"foo\"" "\"bar\"" "\"baz\"")
                 (mc "'(\"foo\" \"bar\" \"baz\")" :normal t)))

  (should (equal '("\"foo\"" "\"bar\"" "\"baz\"")
                 (mc "[\"foo\" \"bar\" \"baz\"]" :normal t)))

  (should (equal '("\"foo\"")
                 (mc "\"foo\"" :normal t)))

  (should (equal '(":foo")
                 (mc ":foo" :normal t)))

  (should (equal '("1" "2" "3")
                 (mc '(1 2 3) :normal nil)))

  ;;explicit mode
  (should (equal '("(1 2 3)")
                 (mc "'(1 2 3)" :explicit t)))

  (should (equal '("[1 2 3]")
                 (mc "[1 2 3]" :explicit t)))

  (should (equal '("(\"foo\" \"bar\" \"baz\")")
                 (mc "'(\"foo\" \"bar\" \"baz\")" :explicit t)))

  (should (equal '("[\"foo\" \"bar\" \"baz\"]")
                 (mc "[\"foo\" \"bar\" \"baz\"]" :explicit t)))

  (should (equal '("\"foo\"")
                 (mc "\"foo\"" :explicit t)))

  (should (equal '(":foo")
                 (mc ":foo" :explicit t)))

  (should (equal '("1")
                 (mc '(1) :explicit nil)))

  ;; situation should never happen because subsequent(non-initial) calls to make-candidate in explicit mode should only be lists of size 1
  (should (equal '("1")
                 (mc '(1 2 3) :explicit nil))))

(ert-deftest evalator-context-elisp-transform-candidates-tests ()
  (should (equal '("1" "2" "3" "4")
                 (tc '("0" "1" "2" "3") nil "(+ 1 Ⓔ)" :normal)))

  (should (equal '("\"foo1\"" "\"bar1\"" "\"baz1\"")
                 (tc '("\"foo\"" "\"bar\"" "\"baz\"") nil "(concat Ⓔ \"1\")" :normal)))

  (should (equal '("4")
                 (tc '("0" "1" "2" "3") '("1" "3") "(cl-reduce '+ 'Ⓔ)" :normal)))

  (should (equal '("\"foobar\"")
                 (tc '("\"foo\"" "\"bar\"") '("\"foo\"" "\"bar\"") "(concat Ⓔ0 Ⓔ1)" :normal))))
