;;; evalator-context-elisp.el --- Elisp evaluation context definition for Evalator
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


(require 'cl-lib)
(require 'eieio)
(require 'evalator-context)

(defvar evalator-context-elisp-special-arg nil)

(defvar evalator-context-elisp
  (make-instance
   'evalator-context
   
   :name
   "elisp"

   :special-arg
   'evalator-context-elisp-special-arg
   
   :init
   (lambda () nil)

   :make-equiv-expr
   'evalator-context-elisp-make-equiv-expr

   :make-candidates
   'evalator-context-elisp-make-candidates
   

   :transform-candidates
   'evalator-context-elisp-transform-candidates))

(defun evalator-context-elisp-get-special-arg ()
  "Return special arg from elisp context."
  (evalator-context-get-special-arg evalator-context-elisp))

;; This function is gross because I can't use a regex matching function
;; I'd like to, but this function is called from within a regex match handler,
;; Calling one from within another produces unexpected behavior.
(defun evalator-context-elisp-numbered-arg-num (str)
  ""
  (let* ((first-char-str (cl-subseq str 0 1))
         (not-num-p (and (equal 0 (string-to-number first-char-str))
                         (not (equal "0" first-char-str)))))
    (if not-num-p
        (evalator-context-elisp-numbered-arg-num (cl-subseq str 1))
      (string-to-number str))))

(defun evalator-context-elisp-numbered-arg-pattern (&optional quote-p)
  ""
  (let ((frmt (if quote-p "'?%s[0-9]+" "%s[0-9]+")))
    (format frmt (evalator-context-elisp-get-special-arg))))

(defun evalator-context-elisp-make-equiv-expr (exprs)
  "Create an equivalent expression string from EXPRS.

Accepts a list of expressions, EXPRS, and return the equivalent
expression by substituting any special args with the expression
before it."
  (let* ((pattern-numbered (evalator-context-elisp-numbered-arg-pattern t))
         (pattern-identity (format "'?%s" (evalator-context-elisp-get-special-arg)))
         (match-f (lambda (str m)
                    (concat "(elt "
                            str
                            " "
                            (number-to-string (evalator-context-elisp-numbered-arg-num m))
                            ")")))
         (sub (lambda (expr-acc expr)
                (let* ((match-f (apply-partially match-f expr-acc))
                       
                       (num-args-replaced
                        (replace-regexp-in-string pattern-numbered match-f expr t))

                       (num-and-identity-args-replaced
                        (replace-regexp-in-string pattern-identity expr-acc num-args-replaced t t)))
                  num-and-identity-args-replaced))))
    (cl-reduce sub exprs)))

(defun evalator-context-elisp-subst-numbered-special-args (expr-str c)
  ""
  (let ((pattern (evalator-context-elisp-numbered-arg-pattern))
        (match-f (lambda (m)
                   (prin1-to-string (elt c (string-to-number (cl-subseq m 1)))))))
    (replace-regexp-in-string pattern match-f expr-str t t)))

(defun evalator-context-elisp-subst-identity-special-args (expr-str c)
  ""
  (let ((sa (evalator-context-elisp-get-special-arg)))
    (replace-regexp-in-string sa (prin1-to-string c) expr-str t t)))

(defun evalator-context-elisp-subst-special-args (expr-str c)
  ""
  ;;TODO this is ugly
  (evalator-context-elisp-subst-identity-special-args
   (evalator-context-elisp-subst-numbered-special-args expr-str c) c))

(defun evalator-context-elisp-make-candidates (input mode initial-p)
  "Convert INPUT into a valid list of helm candidates.

In other words, a list of the stringified representation of the
input.  How INPUT is converted depends on both the MODE argument and
the INITIAL-P flag.  If INITIAL-P is non-nil then it is assumed that
INPUT came from user input and first needs to be read and evaluated to
an elisp object.  If INITIAL-P is nil then it is treated as an elisp
object.  If MODE is :explicit then the function will always return a
candidate list of one element.  If MODE is some other value then the
function will return a candidate list equivalent to the size of the
input object.  That means scalars will be returned in a size 1
candidates list.  Vectors and lists will be returned in a candidates
list whose size is equal to the size of the collection."
  (let* ((data (if initial-p (eval (read input)) input))
         (to-obj-string (lambda (x)
                          (prin1-to-string x))))
    (cond
     ((equal :explicit mode) (if initial-p
                                 (list (funcall to-obj-string data))
                               (list (funcall to-obj-string (car data)))))
     ((and (not (stringp data)) (sequencep data)) (mapcar to-obj-string data))
     (t (list (funcall to-obj-string data))))))

(defun evalator-context-elisp-transform-candidates (cands expr-str mode &optional collect-p)
  ""
  (let ((cands-xfrmd (if collect-p
                         (evalator-context-elisp-eval expr-str (mapcar 'read cands))
                       (mapcar (lambda (c)
                                 (evalator-context-elisp-eval expr-str (read c))) cands))))
    (evalator-context-elisp-make-candidates cands-xfrmd mode nil)))

(defun evalator-context-elisp-eval (expr-str c)
  "Evaluate the expression string, EXPR-STR.
Substitutes any special args in EXPR-STR, with candidate C(or the
element within C) and evaluates the final expression."
  (eval (read (evalator-context-elisp-subst-special-args expr-str c))))

(provide 'evalator-context-elisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-context-elisp.el ends here
