;;; evalator-utils-test.el --- Tests for evalator-utils.el
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


(require 'evalator-utils)
(require 'noflet)

(ert-deftest evalator-utils-put!-test ()
  (let ((plst '(:foo :bar)))
    (evalator-utils-put! plst :foo :baz)
    (should (equal '(:foo :baz) plst))))

(ert-deftest evalator-utils-get-file-string ()
  (noflet ((insert-file-contents (filepath) (insert "foo")))
    (should (equal "foo" (evalator-utils-get-file-string "/dummy/file/path")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-utils-test.el ends here
