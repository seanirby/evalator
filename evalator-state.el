;;; evalator-state.el --- evalator-state var and helpers
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


(require 'helm)
(require 'evalator-utils)
(require 'evalator-context)
(require 'evalator-elisp)

(defvar evalator-state-default (list :context       'evalator-elisp-context
                                     :mode          :normal
                                     :history       []
                                     :history-index -1))

(defvar evalator-state (copy-sequence evalator-state-default))

(defun evalator-state-init (&optional mode context)
  "Initialize the `evalator-state' var.

First, `evalator-state-default' is copied to `evalator-state'.
Then, the state's `:mode' is set to MODE if MODE is non-nil.
Finally the function defined in the context's `:init' slot is called
to perform any context specific initialization.
"
  (evalator-elisp-context) ;;Initialize elisp context if not already.
  (setq evalator-state (copy-sequence evalator-state-default))
  (evalator-utils-put! evalator-state :context (evalator-context-get context))
  (when mode
    (evalator-utils-put! evalator-state :mode mode))
  (funcall (slot-value (evalator-state-context) :init)))

(defun evalator-state-context ()
  "Return the state's context object."
  (funcall (plist-get evalator-state :context)))

(provide 'evalator-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-state.el ends here
