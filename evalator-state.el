(require 'helm)
(require 'evalator-context-elisp)

(defvar evalator-state-default (list :context       evalator-context-elisp
                                     :mode          :normal
                                     :seed          nil
                                     :history       []
                                     :history-index -1))

(defvar evalator-state (copy-sequence evalator-state-default))

(defun evalator-state-init ()
  "Set state back to initial value."
  (setq evalator-state (copy-sequence evalator-state-default)))

(provide 'evalator-state)
