;;; evalator-context-elisp-test.el --- Test for evalator-context-elisp-test.el
;; 
;; Author: Sean Irby
;; Copyright © , Sean Irby
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; This file is not a part of GNU Emacs
;; 
;;; Commentary:
;; 
;;; Code:


(require 'ert)
(require 'evalator-context-elisp)

(defalias 'ssa 'evalator-context-elisp-subst-special-args)
(defalias 'mc 'evalator-context-elisp-make-candidates)
(defalias 'tc 'evalator-context-elisp-transform-candidates)

(ert-deftest evalator-context-elisp-get-special-arg-tests ()
  (let ((evalator-context-special-arg-default "@"))
    (let ((evalator-context-elisp-special-arg "$"))
          (should (equal "$"
                         (evalator-context-elisp-get-special-arg))))
    (should (equal "@"
                       (evalator-context-elisp-get-special-arg)))))

(ert-deftest evalator-context-elisp-numbered-arg-num-tests ()
  (should (equal 123
                 (evalator-context-elisp-numbered-arg-num "@123")))

  (should (equal 123
                 ;; Not expected input
                 (evalator-context-elisp-numbered-arg-num "123")))

  (should (equal 123
                 ;; Not expected input
                 (evalator-context-elisp-numbered-arg-num "foo bar baz @123"))))

(ert-deftest evalator-context-elisp-numbered-arg-pattern-tests ()
  (let ((evalator-context-elisp-special-arg "@"))
    (should (equal "@[0-9]+"
                   (evalator-context-elisp-numbered-arg-pattern)))
    (should (equal "'?@[0-9]+"
                   (evalator-context-elisp-numbered-arg-pattern t)))))

(ert-deftest evalator-context-elisp-make-equiv-expr-tests ()
  (let ((evalator-context-elisp-special-arg "@"))
    (should (equal "(cons (elt (list 1 2 3) 0) (list 1 2 3))"
                   (evalator-context-elisp-make-equiv-expr '("(list 1 2 3)" "(cons @0 '@)"))))))

(ert-deftest evalator-context-elisp-subst-numbered-special-args-tests ()
  (let ((evalator-context-elisp-special-arg "@"))
    (should (equal "(+ @ 3 7)"
                   (evalator-context-elisp-subst-numbered-special-args "(+ @ @0 @1)" '(3 7))))))

(ert-deftest evalator-context-elisp-subst-identity-special-args-tests ()
  (let ((evalator-context-elisp-special-arg "@"))
    (should (equal "(+ 1 1)"
                   (evalator-context-elisp-subst-identity-special-args "(+ @ @)" 1)))))

(ert-deftest evalator-context-elisp-subst-special-args-tests ()
  (let ((evalator-context-elisp-special-arg "@"))
    (should (equal "'(+ 1 1)"
                   (ssa "'(+ 1 1)" nil)))

    (should (equal "'(+ 1 1)"
                   (ssa "'(+ 1 @0)" '(1 2 3))))

    (should (equal "'(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1)))))"
                   (ssa "'(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 @0)))))" [1 2 3])))

    (should (equal "1"
                   (ssa "@" 1)))))

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
                 (mc ":foo" :explicit))))

(ert-deftest evalator-context-elisp-transform-candidates-tests ()
  (let ((evalator-context-elisp-special-arg "@"))
    (should (equal '("1" "2" "3" "4")
                   (tc '("0" "1" "2" "3") "(+ 1 @)" nil)))

    (should (equal '("\"foo1\"" "\"bar1\"" "\"baz1\"")
                   (tc '("\"foo\"" "\"bar\"" "\"baz\"") "(concat @ \"1\")" nil)))

    (should (equal '("4")
                   (tc '("1" "3") "(cl-reduce '+ '@)" t)))

    (should (equal '("\"foobar\"")
                   (tc '("\"foo\"" "\"bar\"") "(concat @0 @1)" t)))))

(ert-deftest evalator-context-elisp-eval-tests ()
  (should (equal 2
                 (evalator-context-elisp-eval "(+ 1 1)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context-elisp-test.el ends here
