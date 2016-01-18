;;; evalator-faces.el --- Evalator faces
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


(defgroup evalator-faces nil
  "Customize the appearance of evalator."
  :prefix "evalator-"
  :group 'faces
  :group 'evalator)

(defface evalator-success
  '((((background dark))
     :background "green4"
     :foreground "white"
     )
    (((background light))
     :background "green4"
     :foreground "white"
     ))
  "Face for source header in the evalator buffer."
  :group 'evalator-faces)

(defface evalator-error
  '((((background dark))
     :background "red4"
     :foreground "white"
     )
    (((background light))
     :background "red4"
     :foreground "white"
     ))
  "Face for source header in the evalator buffer."
  :group 'evalator-faces)

(provide 'evalator-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-faces.el ends here
