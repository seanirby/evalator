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

(setq evalator-context-elisp-special-arg "Ⓔ")
(defalias 'ssa 'evalator-context-elisp-subst-special-args)
(defalias 'mc 'evalator-context-elisp-make-candidates)
(defalias 'tc 'evalator-context-elisp-transform-candidates)

(ert-deftest evalator-context-elisp-eval ()
  (should (equal 2
                 (evalator-context-elisp-eval "(+ 1 1)" nil))))

(ert-deftest evalator-context-elisp-subst-special-args ()
  (should (equal "'(+ 1 1)"
                 (ssa "'(+ 1 1)" nil)))

  (should (equal "'(+ 1 1)"
                 (ssa "'(+ 1 Ⓔ)" 1)))

  (should (equal "'(+ 1 1)"
                 (ssa "'(+ 1 Ⓔ0)" '(1 2 3))))

  (should (equal "'(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1)))))"
                 (ssa "'(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 Ⓔ0)))))" [1 2 3])))

  (should (equal "1"
                 (ssa "Ⓔ" 1))))

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
                 (tc '("0" "1" "2" "3") "(+ 1 Ⓔ)" :normal)))

  (should (equal '("\"foo1\"" "\"bar1\"" "\"baz1\"")
                 (tc '("\"foo\"" "\"bar\"" "\"baz\"") "(concat Ⓔ \"1\")" :normal)))

  (should (equal '("4")
                 (tc '("1" "3") "(cl-reduce '+ 'Ⓔ)" :normal t)))

  (should (equal '("\"foobar\"")
                 (tc '("\"foo\"" "\"bar\"") "(concat Ⓔ0 Ⓔ1)" :normal t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context-elisp-test.el ends here
