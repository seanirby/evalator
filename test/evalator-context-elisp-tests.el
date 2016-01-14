(require 'ert)
(require 'evalator-context-elisp)

(setq evalator-context-special-arg-default "Ⓔ")
(defalias 'ssa 'evalator-context-elisp-substitute-special-args)
(defalias 'mc 'evalator-context-elisp-make-candidates)
(defalias 'tc 'evalator-context-elisp-transform-candidates)

(ert-deftest evalator-context-elisp-eval ()
  (should (equal 2
                 (evalator-context-elisp-eval '(+ 1 1) nil))))

(ert-deftest evalator-context-elisp-substitute-special-args ()
  (should (equal '(+ 1 1)
                 (ssa '(+ 1 1) nil)))

  (should (equal '(+ 1 1)
                 (ssa '(+ 1 Ⓔ) 1)))

  (should (equal '(+ 1 1)
                 (ssa '(+ 1 Ⓔ0) '(1 2 3))))

  (should (equal '(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1)))))
                 (ssa '(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 Ⓔ0))))) [1 2 3])))

  (should (equal 1
                 (ssa 'Ⓔ 1))))

(ert-deftest evalator-context-elisp-make-candidates-tests ()
  ;;normal mode
  (should (equal '("1" "2" "3")
                 (mc "'(1 2 3)" :normal)))

  (should (equal '("1" "2" "3")
                 (mc "[1 2 3]" :normal)))

  (should (equal '("\"foo\"" "\"bar\"" "\"baz\"")
                 (mc "'(\"foo\" \"bar\" \"baz\")" :normal)))

  (should (equal '("\"foo\"" "\"bar\"" "\"baz\"")
                 (mc "[\"foo\" \"bar\" \"baz\"]" :normal)))

  (should (equal '("\"foo\"")
                 (mc "\"foo\"" :normal)))

  (should (equal '(":foo")
                 (mc ":foo" :normal)))

  (should (equal '("1" "2" "3")
                 (mc '(1 2 3) :normal t)))

  ;;explicit mode
  (should (equal '("(1 2 3)")
                 (mc "'(1 2 3)" :explicit)))

  (should (equal '("[1 2 3]")
                 (mc "[1 2 3]" :explicit)))

  (should (equal '("(\"foo\" \"bar\" \"baz\")")
                 (mc "'(\"foo\" \"bar\" \"baz\")" :explicit)))

  (should (equal '("[\"foo\" \"bar\" \"baz\"]")
                 (mc "[\"foo\" \"bar\" \"baz\"]" :explicit)))

  (should (equal '("\"foo\"")
                 (mc "\"foo\"" :explicit)))

  (should (equal '(":foo")
                 (mc ":foo" :explicit)))

  (should (equal '("1")
                 (mc '(1) :explicit t)))

  ;; situation should never happen because subsequent(non-initial) calls to make-candidate in explicit mode should only be lists of size 1
  (should (equal '("1")
                 (mc '(1 2 3) :explicit t))))

(ert-deftest evalator-context-elisp-transform-candidates-tests ()
  (should (equal '("1" "2" "3" "4")
                 (tc '("0" "1" "2" "3") nil "(+ 1 Ⓔ)" :normal)))

  (should (equal '("\"foo1\"" "\"bar1\"" "\"baz1\"")
                 (tc '("\"foo\"" "\"bar\"" "\"baz\"") nil "(concat Ⓔ \"1\")" :normal)))

  (should (equal '("4")
                 (tc '("0" "1" "2" "3") '("1" "3") "(reduce '+ Ⓔ)" :normal)))

  (should (equal '("\"foobar\"")
                 (tc '("\"foo\"" "\"bar\"") '("\"foo\"" "\"bar\"") "(concat Ⓔ0 Ⓔ1)" :normal))))
