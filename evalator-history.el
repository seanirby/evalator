;;; evalator-history.el --- History helpers
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


(require 'cl-lib)
(require 'evalator-utils)
(require 'evalator-state)
(require 'helm)

(defun evalator-history ()
  "Return history vector."
  (plist-get evalator-state :history))

(defun evalator-history-index ()
  "Return current history index."
  (plist-get evalator-state :history-index))

(defun evalator-history-push! (candidates expression)
  "Push the CANDIDATES and EXPRESSION onto history.
Increments the history index."
  (evalator-utils-put! evalator-state
                       :history
                       (vconcat (cl-subseq (evalator-history) 0 (+ 1 (evalator-history-index)))
                                (list (list :candidates candidates :expression expression))))
  (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index))))

(defun evalator-history-current (&optional k)
  "Retrieve active history element.  Accepts an optional keyword K."
  (let ((h (elt (evalator-history) (evalator-history-index))))
    (if k (plist-get h k) h)))

(defun evalator-history-expression-chain ()
  "Return a list of all expressions in history except for the first.
The first expression is always an empty string so it is ignored."
  (cdr (mapcar
        (lambda (h)
          (plist-get h :expression))
        (plist-get evalator-state :history))))

(provide 'evalator-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-history.el ends here
