;;; evalator-config.el --- User configuration vars for evalator
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

(defvar evalator-config-mode-context-alist nil
  "An association list that consists of (MAJOR-MODE . CONTEXT) pairs.

When an evalator session is started, this list is used to determine
the evaluation context to use.  If the buffer's current major-mode
matches a major-mode in this list, then the associated context will be
used.
")

(defvar evalator-config-prompt-f
  (lambda ()
    (format "%s of %s" (+ 1 (evalator-history-index)) (length (evalator-history))))
  "Var's value is a function used to generate the evalator prompt.")

(provide 'evalator-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-config.el ends here
