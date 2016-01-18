;;; evalator-context.el --- Definition for evalator-context class
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


(require 'eieio)

(defvar evalator-context-special-arg-default "Ⓔ") ;; x24ba

;; References to data types in the docstrings below are assumed to be elisp types.
(defclass evalator-context ()
  ((name
    :initarg :name
    :custom string
    :documentation
    "Name of the evaluation context (elisp, cider, etc..)")

   (special-arg
    :initarg :special-arg
    :custom string
    :documentation
    "Special arg used for substitution in evalator expressions.")
   
   (init
    :initarg :init
    :custom function
    :documentation
    "Performs any setup needed before any context evaluation functions
    are called. All functions below are context evaluation functions.")

   (make-equiv-expr
    :initarg :make-equiv-expr
    :custom function
    :documentation
    "")

   (make-candidates
    :initarg :make-candidates
    :custom function
    :documentation
    "Function evaluates the input data and transforms it so that it
    can be used as the ':candidates' argument for building a helm
    source. Function should accept a string or list of strings.
    Function should return a list of strings")

   (transform-candidates
    :initarg :transform-candidates
    :custom function
    :documentation
    "Function that applies an expression over a selection of
    candidates.  If no candidates are marked, then the selection will
    be all candidates.  Otherwise, the selection will be the marked
    candidates.  Function should accept all current candidates, all
    marked candidates, and an expression string as arguments. Function
    should return a list of strings.  Function should throw an error
    if the transformation fails.")))

(defmethod evalator-context-get-special-arg ((context evalator-context))
  (or (eval (slot-value context :special-arg))
      evalator-context-special-arg-default))

(provide 'evalator-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context.el ends here
