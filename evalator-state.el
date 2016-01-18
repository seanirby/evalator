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
(require 'evalator-context-elisp)

(defvar evalator-state-default (list :context       evalator-context-elisp
                                     :mode          :normal
                                     :history       []
                                     :history-index -1))

(defvar evalator-state (copy-sequence evalator-state-default))

(defun evalator-state-init (&optional mode)
  "Set `evalator-state' to the value of `evalator-state-default'.
Sets the state's MODE if necessary and performs any context initialization"
  (setq evalator-state (copy-sequence evalator-state-default))
  (when mode (evalator-utils-put! evalator-state :mode mode))
  (funcall (slot-value (plist-get evalator-state :context) :init)))

(provide 'evalator-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-state.el ends here
