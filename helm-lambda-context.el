(require 'eieio)


;; References to data types in the docstrings below are assumed to be elisp types.
(defclass helm-lambda-context ()
  ((name
    :initarg :name
    :custom string
    :documentation
    "Name of the evaluation context (elisp, clojure, etc..)")

   (stringify-listify
    :initarg :stringify-listify
    :custom function
    :documentation
    "Function transforms input data so that it can be used as the
    ':candidates' argument for building a helm source. Function should
    accept a string.  Function should return a list of strings")

   (transform-candidates-try
    :initarg :transform-candidates-try
    :custom function
    :documentation
    "Same signature as 'transform candidates' except this function
    wraps the call to 'transform candidates' so all candidates will be
    returned if it fails.")

   (transform-candidates
    :initarg :transform-candidates
    :custom function
    :documentation
    "Function that attempts to apply an expression over a selection
    of candidates.  If no candidates are marked, then the
    selection will be all candidates.  Otherwise, the selection
    will be the marked canidates.  Function should accept an
    evaluation context, all current candidates, all
    marked candidates, and an expression string as arguments.
    Function should return a list of strings.")

   (apply-expression
    :initarg :apply-expression
    :custom function
    :documentation
    "TODO fill in")))

(provide 'helm-lambda-context)
