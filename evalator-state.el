(require 'helm)
(require 'evalator-context-elisp)

(defvar evalator-state-default (list :context       evalator-context-elisp
                                     :mode          :normal
                                     :history       []
                                     :history-index -1))

(defvar evalator-state (copy-sequence evalator-state-default))

(defun evalator-state-init (&optional mode)
  (setq evalator-state (copy-sequence evalator-state-default))
  (when mode (evalator-utils-put! evalator-state :mode mode))
  "Set state back to initial value."
  (funcall (slot-value (plist-get evalator-state :context) :init)))

(provide 'evalator-state)
