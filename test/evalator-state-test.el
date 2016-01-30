;;; evalator-state-test.el --- Tests for evalator-state.el
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
(require 'el-mock)
(require 'evalator-state)

(ert-deftest evalator-state-init-test ()
  "Tests that 'evalator state' is initialized with the value of
'evalator-state-default'"
  (let ((evalator-state-default (copy-sequence evalator-state-default))
        (evalator-state nil))
    (progn
      (evalator-state-init :normal)
      (should (equal evalator-state-default evalator-state))

      (evalator-state-init :explicit)
      (should-not (equal evalator-state-default evalator-state))
      (should (equal :explicit (plist-get evalator-state :mode))))))

(ert-deftest evalator-state-context-test ()
  (let* ((evalator-state (list :context "foo")))
    (should (equal "foo"
                   (evalator-state-context)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-state-test.el ends here
