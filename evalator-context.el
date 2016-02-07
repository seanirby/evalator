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
(require 'evalator-config)

(defvar evalator-context-special-arg-default "Ⓔ") ;; Unicode character x24ba

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
    "() => t or nil

    Performs any setup needed before any context evaluation functions
    are called. All slot functions below are context evaluation
    functions.  If init returns non-nil then initialization was
    successful and evalator can be started.  Otherwise, evalator is
    aborted.  Function should print an error message if initialization
    failed.")

   (make-equiv-expr
    :initarg :make-equiv-expr
    :custom function
    :documentation
    "(exprs) => string

    This function accepts a single argument, EXPRS, which is the list
    of expression strings used in the most recent evalator session.
    It should combine them and return a single expression string as a
    result.")

   (make-candidates
    :initarg :make-candidates
    :custom function
    :documentation
    "(input mode initial-p) => cons

    Function converts INPUT into a valid list of helm candidates.  In
    other words, a list of the stringified representation of the
    input.  How INPUT is converted depends on both the MODE argument
    and the INITIAL-P flag.

    If INITIAL-P is non-nil then it is assumed that INPUT came from
    user input and first needs to be read and evaluated to an elisp
    object.  If INITIAL-P is nil then it is treated as an elisp
    object.  If MODE is :explicit then the function will always return
    a candidate list of one element.  If MODE is some other value then
    the function will return a candidate list equivalent to the size
    of the input object.  That means scalars will be returned in a
    size 1 candidates list.  Vectors and lists will be returned in a
    candidates list whose size is equal to the size of the
    collection.")

   (transform-candidates
    :initarg :transform-candidates
    :custom function
    :documentation
    "(cands expr-str collect-p) => cons

    Function accepts a list of candidates, CANDS, and transforms it
    according to the expression string EXPR-STR.  How CANDS is
    transformed depends on the flag COLLECT-P.  If COLLECT-P is
    non-nil then EXPR-STR will be evaluated on the entire CANDS list.
    If COLLECT-P is nil then EXPR-STR will be evaluated on each
    candidate in CANDS.  The result is then processed so it's a valid
    helm candidate list then returned.")))

(defmethod evalator-context-get-special-arg ((context evalator-context))
  (or (eval (slot-value context :special-arg))
      evalator-context-special-arg-default))

(defun evalator-context-get (&optional context)
  "Find an evaluation context to use for an evalator session.

If CONTEXT is non-nil, then the result of calling CONTEXT's function
definition will be used as the session's evaluation context.

If CONTEXT is nil, then the current buffer's major mode will be
searched for in `evalator-config-mode-context-alist'.  If a match is found,
the context associated with that major mode is used in the evalator
session.  If no match is found, an elisp evaluation context is used
instead.
"
  (let* ((mm (buffer-local-value 'major-mode (current-buffer)))
         (context-f-sym (or context
                            (cdr (assoc mm evalator-config-mode-context-alist))
                            'evalator-elisp-context))
         (context-f (symbol-function context-f-sym)))
    (if (autoloadp context-f)
        (progn
          (autoload-do-load context-f context-f-sym)
          (funcall (symbol-function context-f-sym)))
      (funcall context-f))
    context-f-sym))

(provide 'evalator-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context.el ends here
