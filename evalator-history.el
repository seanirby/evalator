(require 'evalator-state)
(require 'helm)

(defun evalator-history ()
  "Return history"
  (plist-get evalator-state :history))

(defun evalator-history-index ()
  "Return current history index"
  (plist-get evalator-state :history-index))

(defun evalator-history-push! (data)
  "Push the current source and expression onto history"
  (evalator-utils-put! evalator-state
                       :history
                       (vconcat (subseq (evalator-history) 0 (+ 1 (evalator-history-index)))
                                (list (list :source data :expression helm-pattern))))
  (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index))))

(defun evalator-history-current (&optional k)
  "Retrieve active history element.  Accepts an optional key."
  (let ((h (elt (evalator-history) (evalator-history-index))))
    (if k (plist-get h k) h)))

(defun evalator-history-load ()
  "Quits the current evalator session and loads a new one."
  (let* ((source (evalator-history-current :source))
         (candidates (helm-get-candidates source))
         (f (lambda (candidates _) (evalator :candidates candidates
                                             :initp      nil
                                             :hist-pushp nil))))
    (helm-exit-and-execute-action (apply-partially f candidates))))

(provide 'evalator-history)
