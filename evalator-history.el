(require 'evalator-utils)
(require 'evalator-state)
(require 'helm)

(defun evalator-history ()
  "Return history"
  (plist-get evalator-state :history))

(defun evalator-history-index ()
  "Return current history index"
  (plist-get evalator-state :history-index))

(defun evalator-history-push! (source expression)
  "Push the source and expression onto history.  Increment the history
index."
  (evalator-utils-put! evalator-state
                       :history
                       (vconcat (subseq (evalator-history) 0 (+ 1 (evalator-history-index)))
                                (list (list :source source :expression expression))))
  (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index))))

(defun evalator-history-current (&optional k)
  "Retrieve active history element.  Accepts an optional key."
  (let ((h (elt (evalator-history) (evalator-history-index))))
    (if k (plist-get h k) h)))

(provide 'evalator-history)
