(require 'helm)
(require 'evalator-context-elisp)

(defvar evalator-state-default (list :context       evalator-context-elisp
                                     :mode          :normal
                                     :history       []
                                     :history-index -1))

(defvar evalator-state (copy-sequence evalator-state-default))

(defun evalator-state-init (&optional mode)
  "Sets `evalator-state' to the value of `evalator-state-default',
sets the mode if necessary according to MODE, and calls the evaluation
context's initialize function."
  (setq evalator-state (copy-sequence evalator-state-default))
  (when mode (evalator-utils-put! evalator-state :mode mode))
  (funcall (slot-value (plist-get evalator-state :context) :init)))

(provide 'evalator-state)
