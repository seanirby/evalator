(require 'evalator-context)
(require 'evalator-history)
(require 'evalator-state)
(require 'helm)

;; TODO There's currently a weird bug happening where spamming the history next
;; and previous actions will cause the evalator session to shut down. Has to do with
;; let bindings being nested too deep.
(defun evalator-action-previous ()
  "Go to the next history state and update the evalator session."
  (interactive)
  (when (not (equal 0 (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ -1 (evalator-history-index)))
    (evalator-history-load)))

(defun evalator-action-next ()
  "Go to the previous history state and update the evalator session."
  (interactive)
  (when (not (equal (+ -1 (length (evalator-history))) (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index)))
    (evalator-history-load)))

(defun evalator-action-confirm ()
  "Accepts results and starts a new evalator for further
transformation."
  (interactive)
  (let* ((err-handler (lambda ()
                        (with-current-buffer "*evalator*"
                          (message "Can't update, invalid expression")
                          nil)))
         (candidates (evalator-transform-candidates err-handler))
         (f (lambda (candidates _) (evalator :candidates candidates
                                             :initp      nil
                                             :hist-pushp t))))
    (when candidates
      (helm-exit-and-execute-action (apply-partially f candidates)))))

(defun evalator-action-insert-arg ()
  (interactive)
  ;; Need to get this from context instead
  (insert evalator-context-special-arg-default))

(provide 'evalator-action)
