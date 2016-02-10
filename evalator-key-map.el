;;; evalator-key-map.el --- Key map for evalator actions
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

(defvar evalator-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     'evalator-action-confirm-make-or-transform)
    (define-key map (kbd "C-c ;")   'evalator-action-insert-special-arg)
    (define-key map (kbd "C-;")     'evalator-action-insert-special-arg)
    (define-key map (kbd "C-c C-c") 'evalator-action-confirm-transform-collect)
    (define-key map (kbd "C-c C-e") 'evalator-action-execute-in-elisp)
    (define-key map (kbd "C-j")     'evalator-action-next)
    (define-key map (kbd "C-l")     'evalator-action-previous)
    map))

(provide 'evalator-key-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-key-map.el ends here
