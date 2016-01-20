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

(ert-deftest evalator-prompt-f-tests ()
  (noflet ((evalator-history-index () 0)
           (evalator-history () '(length of 3)))
          (should (equal "1 of 3"
                         (funcall evalator-prompt-f)))))

(ert-deftest evalator-action-previous-and-next-tests ()
  (let ((evalator-state (list :history [(:candidates '())
                                        (:candidates '())]
                              :history-index 0)))
    (noflet ((evalator-unmark-all () nil)
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
  (let ((helm-pattern "foo"))
    ;;Successful transformation
    (with-mock
     (mock (evalator-candidate-transformer * *) => t)
     (mock (evalator-history-push! * *) :times 1)
     (mock (evalator-unmark-all) :times 1)
     (mock (helm-set-pattern "") :times 1 => t)
     (evalator-action-confirm))
    ;;Unsuccessful transformation
    (with-mock
     (mock (evalator-candidate-transformer * *) => nil)
     (should (equal nil
                    (evalator-action-confirm))))))

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

(ert-deftest evalator-persistent-help-test ()
  (let ((evalator-key-map (list 'evalator-action-previous           "C-l"
                                'evalator-action-confirm            "RET"
                                'evalator-action-insert-special-arg "C-;")))
    (noflet ((where-is-internal (command key-map _) (plist-get key-map command))
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

(ert-deftest evalator-try-context-candidate-f-test ()
  ;; successful call
  (let ((helm-pattern "(non-empty expression)")
        (context-f   (lambda (&rest args) t)))
    (with-mock
     (mock (evalator-flash :success) :times 1)
     (evalator-try-context-candidate-f context-f nil nil)))
  ;; trigger error because of empty pattern
  (let ((helm-pattern "")
        (context-f    (lambda (&rest args) t)))
    (with-mock
     (mock (evalator-flash :error) :times 1)
     (mock (evalator-history-current :candidates) :times 1 => t)
     (evalator-try-context-candidate-f context-f nil nil)))
  ;; error handler gets called
  (let ((helm-pattern "(non-empty expression)")
        (context-f    (lambda (&rest args) (signal 'evalator-error '(""))))
        (err-handler  (lambda (_) t)))
    (with-mock
     ;; Flashes :success then :error
     (mock (evalator-flash *) :times 2)
     (evalator-try-context-candidate-f context-f nil err-handler))))

;; TODO
(ert-deftest evalator-candidate-transformer ())

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
    (noflet ((evalator-state-init (_)
                                  (setq state-init-p t))
             (evalator-history-push! (cands expr)
                                     (setq history (list :candidates cands
                                                         :expression expr)))
             (evalator-build-history-source () '())
             (evalator-build-source (cands mode) `(,cands ,mode))
             (helm (&rest args) (cadr args)))

            ;;helm should be called with the result from evalator-build-source as the :source
            (should (equal '(() (("foo") :explicit))
                           (evalator :explicit)))

            (should state-init-p)

            (should (equal (list :candidates '("foo")
                                 :expression "")
                           history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-test.el ends here
