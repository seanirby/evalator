;;; evalator-test.el --- Tests for evalator.el
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


(require 'noflet)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'evalator)

(ert-deftest evalator-action-previous-and-next-tests ()
  (let ((evalator-state (list :history [(:candidates '())
                                        (:candidates '())]
                              :history-index 0)))
    (noflet ((helm-unmark-all () nil)
             (helm-set-pattern (_) nil)
             (helm-update () nil))
            
            (evalator-action-previous)
            (should (equal 0 (evalator-history-index)))

            (evalator-action-next)
            (should (equal 1 (evalator-history-index)))

            (evalator-action-next)
            (should (equal 1 (evalator-history-index)))

            (evalator-action-previous)
            (should (equal 0 (evalator-history-index))))))

(ert-deftest evalator-action-execute-in-elisp-test ()
  (let ((helm-pattern "@ !"))
    (noflet ((evalator-context-get-special-arg (arg) arg))
            (with-mock
             (stub evalator-elisp-context => "!")
             (stub evalator-state-context => "@")
             (stub evalator-get-candidates => '("cand-1"))
             (mock (evalator-elisp-transform-candidates '("cand-1") "! !" nil) => '("cand-1-xfrmd"))
             (mock (message "(\"cand-1-xfrmd\")") => t)
             (evalator-action-execute-in-elisp))
            (with-mock
             (stub evalator-elisp-context => "@")
             (stub evalator-state-context => "@")
             (stub evalator-get-candidates => '("cand-1"))
             (mock (evalator-elisp-transform-candidates '("cand-1") "@ !" nil) => '("cand-1-xfrmd"))
             (mock (message "(\"cand-1-xfrmd\")") => t)
             (evalator-action-execute-in-elisp))
            (with-mock
             (stub evalator-elisp-context)
             (stub evalator-state-context)
             (stub evalator-get-candidates)
             (stub evalator-elisp-transform-candidates => (signal 'evalator-error '("error")))
             (stub evalator-message => nil)
             (stub error => t)
             (evalator-action-execute-in-elisp))  )))

(ert-deftest evalator-action-confirm-make-or-transform-test ()
  ;;Successful transformation
  (with-mock
   (mock (evalator-candidate-make-or-transform * *) => t)
   (mock (evalator-history-push! * *) :times 1)
   (mock (helm-unmark-all) :times 1)
   (mock (helm-set-pattern "") :times 1 => t)
   (evalator-action-confirm-make-or-transform))
  ;;Unsuccessful transformation
  (with-mock
   (mock (evalator-candidate-make-or-transform * *) => nil)
   (should (equal nil
                  (evalator-action-confirm-make-or-transform)))))

(ert-deftest evalator-action-confirm-transform-collect-test ()
  (let ((helm-pattern "pattern"))
    (with-mock
     (stub slot-value => "f")
     (stub evalator-get-candidates => '("cand-1"))
     (mock (evalator-action-confirm-make-or-transform '("f" (("cand-1") "pattern" t))))
     (evalator-action-confirm-transform-collect))))

(ert-deftest evalator-action-insert-special-arg-test ()
  (with-mock
   (stub evalator-state-context)
   (stub evalator-context-get-special-arg => "@")
   (should (equal "@"
                  (with-temp-buffer
                    (evalator-action-insert-special-arg)
                    (buffer-string))))))

(ert-deftest evalator-flash-test ()
  (with-mock
   (mock (face-remap-add-relative * 'evalator-success) => t)
   (evalator-flash :success))
  (with-mock
   (mock (face-remap-add-relative * 'evalator-error) => t)
   (evalator-flash :error)))

;; TODO
(ert-deftest evalator-unmark-all-test ())

;; TODO
(ert-deftest evalator-marked-candidates-test ())

(ert-deftest evalator-persistent-help-test ()
  (let ((evalator-key-map (list 'evalator-action-previous                  "C-l"
                                'evalator-action-confirm-make-or-transform "RET"
                                'evalator-action-insert-special-arg        "C-c ;")))
    (noflet ((where-is-internal (command key-map _) (plist-get key-map command))
             (key-description (str) str))
            (should (equal (concat "History forward, "
                                   "C-l: History backward, "
                                   "RET: Accept transformation, "
                                   "C-c ;: Insert special arg")
                           (evalator-persistent-help))))))

(ert-deftest evalator-get-candidates-test ()
  (with-mock
   (mock (evalator-history-current :candidates) => '("cand-1" "cand-2"))
   (mock (evalator-marked-candidates) => nil)
   (should (equal '("cand-1" "cand-2")
                  (evalator-get-candidates))))
  (with-mock
   (mock (evalator-history-current :candidates) => '("cand-1" "cand-2"))
   (mock (evalator-marked-candidates) => '("cand-2"))
   (should (equal '("cand-2")
                  (evalator-get-candidates)))))

(ert-deftest evalator-try-context-f-test ()
  ;; successful call
  (let ((helm-pattern "(non-empty expression)")
        (context-f   (lambda (&rest args) t)))
    (with-mock
     (mock (evalator-flash :success) :times 1)
     (evalator-try-context-f context-f nil nil)))
  ;; trigger error because of empty pattern
  (let ((helm-pattern "")
        (context-f    (lambda (&rest args) t)))
    (with-mock
     (mock (evalator-flash :error) :times 1)
     (mock (evalator-history-current :candidates) :times 1 => t)
     (evalator-try-context-f context-f nil nil)))
  ;; error handler gets called
  (let ((helm-pattern "(non-empty expression)")
        (context-f    (lambda (&rest args) (signal 'evalator-error '(""))))
        (err-handler  (lambda (_) t)))
    (with-mock
     ;; Flashes :success then :error
     (mock (evalator-flash *) :times 2)
     (evalator-try-context-f context-f nil err-handler))))

(ert-deftest evalator-make-or-transform-test ()
  (let ((helm-pattern "pattern"))
    (with-mock
     (stub slot-value)
     (mock (evalator-try-context-f "f" "args" "err-handler") => t)
     (evalator-candidate-make-or-transform '("f" "args") "err-handler"))
    (with-mock
     (stub slot-value)
     (stub evalator-history-index => 0)
     (mock (evalator-try-context-f * '("pattern" :normal) *))
     (evalator-candidate-make-or-transform))
    (with-mock
     (stub slot-value)
     (stub evalator-history-index => 1)
     (stub evalator-get-candidates => '("cand-1"))
     (mock (evalator-try-context-f * '(("cand-1") "pattern" nil) *))
     (evalator-candidate-make-or-transform))))

;; Tried to mock the helm-build-sync-source macro but ran into issues
;; This works for now...
(ert-deftest evalator-build-source-test ()
  (let ((args-normal (evalator-build-source nil :normal))
        (args-explicit (evalator-build-source nil :explicit)))
    (should (equal "Evaluation Result"
                   (cdr (assoc 'name args-normal))))
    (should (equal "Evaluation Result(Explicit)"
                   (cdr (assoc 'name args-explicit))))))

(ert-deftest evalator-build-history-source-test ()
  (with-mock
   (stub helm-build-dummy-source => t)
   (evalator-build-history-source)))

(ert-deftest evalator-insert-equiv-expr-test ()
  (with-mock
   (stub slot-value => (lambda (exprs) (car exprs)))
   (stub evalator-history-expression-chain => '("(+ 1 1)"))
   (stub message => "Error message output")
   (stub evalator-state-context)
   (let ((evalator-state  (list :mode :explicit)))
     (with-temp-buffer
       (evalator-insert-equiv-expr)
       (should (equal "(+ 1 1)"
                      (buffer-string)))))
   (let ((evalator-state  (list :mode nil)))
     (should (equal "Error message output"
                    (evalator-insert-equiv-expr))))))

(ert-deftest evalator-resume-test ()
  (with-mock
   (mock (helm-resume "*helm-evalator*"))
   (evalator-resume)))

(ert-deftest evalator-test ()
  (let ((evalator-foo-context "foo")
        (state-init-p nil)
        (history nil)
        (evalator-candidates-initial '("foo")))
    (with-mock
     (mock (evalator-context-get 'evalator-foo-context) => evalator-foo-context)
     (mock (evalator-state-init :explicit "foo") => t)
     (mock (evalator-history-push! * *) :times 1)
     (stub evalator-build-history-source => t)
     (mock (evalator-build-source '("foo") :explicit) => t)
     (mock (helm :sources '(t t)
                 :buffer  "*helm-evalator*"
                 :prompt  "Enter Expression: "
                 :helm-after-update-hook *) => t)
     (should (evalator :explicit 'evalator-foo-context)))))

(ert-deftest evalator-explicit-test ()
  (with-mock
   (mock (evalator :explicit *) => t)
   (evalator-explicit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-test.el ends here
