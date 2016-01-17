(require 'cl-lib)
(require 'evalator-utils)
(require 'evalator-state)
(require 'helm)

(defun evalator-history ()
  "Return history vector"
  (plist-get evalator-state :history))

(defun evalator-history-index ()
  "Return current history index"
  (plist-get evalator-state :history-index))

(defun evalator-history-push! (candidates expression)
  "Push the CANDIDATES and EXPRESSION onto history.  Increment the
history index."
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
  "Returns a list of all expressions in history except for the first
since that is always an empty string."
  (cdr (mapcar
        (lambda (h)
          (plist-get h :expression))
        (plist-get evalator-state :history))))

(provide 'evalator-history)
