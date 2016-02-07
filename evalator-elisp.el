;;; evalator-elisp.el --- Elisp evaluation context definition for Evalator
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

(defvar evalator-elisp-special-arg nil)
(defvar evalator-elisp-context nil)

;;;###autoload
(defun evalator-elisp-context ()
  (if evalator-elisp-context
      evalator-elisp-context
    (progn
      (setq evalator-elisp-context
            (make-instance
             'evalator-context

             :name
             "elisp"

             :special-arg
             'evalator-elisp-special-arg

             :init
             'evalator-elisp-init

             :make-equiv-expr
             'evalator-elisp-make-equiv-expr

             :make-candidates
             'evalator-elisp-make-candidates


             :transform-candidates
             'evalator-elisp-transform-candidates)))))

(defun evalator-elisp-get-special-arg ()
  "Return special arg from elisp context."
  (evalator-context-get-special-arg (evalator-elisp-context)))

(defun evalator-elisp-init ()
  "No Elisp initialization needed since this is EMACS."
  t)

;; This function is gross because I can't use a regex matching function
;; I'd like to, but this function is called from within a regex match handler,
;; Calling one from within another produces unexpected behavior.
(defun evalator-elisp-numbered-arg-num (str)
  "Function initially accepts a string STR of a numbered special arg.
It is called recursively until it has extracted the number following
the special arg."
  (let* ((first-char-str (cl-subseq str 0 1))
         (not-num-p (and (equal 0 (string-to-number first-char-str))
                         (not (equal "0" first-char-str)))))
    (if not-num-p
        (evalator-elisp-numbered-arg-num (cl-subseq str 1))
      (string-to-number str))))

(defun evalator-elisp-numbered-arg-pattern (&optional quote-p)
  "Return the regex pattern used to match numbered special args.
If QUOTE-P is non-nil then a pattern is returned that can also match a
quoted numbered special arg like `'ⒺN'."
  (let ((frmt (if quote-p "'?%s[0-9]+" "%s[0-9]+")))
    (format frmt (evalator-elisp-get-special-arg))))

(defun evalator-elisp-subst-numbered-special-args (expr-str c)
  "Substitute any special args of the form `ⒺN' in EXPR-STR with the Nth element in C."
  (let ((pattern (evalator-elisp-numbered-arg-pattern))
        (match-f (lambda (m)
                   (prin1-to-string (elt c (string-to-number (cl-subseq m 1)))))))
    (replace-regexp-in-string pattern match-f expr-str t t)))

(defun evalator-elisp-subst-identity-special-args (expr-str c)
  "Substitute any special args of the form `Ⓔ' in EXPR-STR with C."
  (let ((sa (evalator-elisp-get-special-arg)))
    (replace-regexp-in-string sa (prin1-to-string c) expr-str t t)))

(defun evalator-elisp-subst-special-args (expr-str c)
  "Substitute any special args in EXPR-STR.
Identity special args like `Ⓔ' are substituted with the value of C.
Numbered special args like `ⒺN' are substituted with the Nth element
in C."
  (let* ((num-args-replaced (evalator-elisp-subst-numbered-special-args expr-str c))
         (num-and-identity-args-replaced
          (evalator-elisp-subst-identity-special-args num-args-replaced c)))
    num-and-identity-args-replaced))

(defun evalator-elisp-eval (expr-str)
  "Evaluate the expression string, EXPR-STR."
  (eval (read expr-str)))

(defun evalator-elisp-make-equiv-expr (exprs)
  "See slot documentation in evalator-context.el."
  (let* ((pattern-numbered (evalator-elisp-numbered-arg-pattern t))
         (pattern-identity (format "'?%s" (evalator-elisp-get-special-arg)))
         (match-f (lambda (str m)
                    (concat "(elt "
                            str
                            " "
                            (number-to-string (evalator-elisp-numbered-arg-num m))
                            ")")))
         (sub (lambda (expr-acc expr)
                (let* ((match-f (apply-partially match-f expr-acc))

                       (num-args-replaced
                        (replace-regexp-in-string pattern-numbered match-f expr t))

                       (num-and-identity-args-replaced
                        (replace-regexp-in-string pattern-identity expr-acc num-args-replaced t t)))
                  num-and-identity-args-replaced))))
    (cl-reduce sub exprs)))

(defun evalator-elisp-make-candidates (input mode)
  "See slot documentation in evalator-context.el."
  (let* ((data (eval (read input)))
         (to-obj-string (lambda (x)
                          (prin1-to-string x))))
    (cond
     ((equal :explicit mode) (list (funcall to-obj-string data)))
     ((and (not (stringp data)) (sequencep data)) (mapcar to-obj-string data))
     (t (list (funcall to-obj-string data))))))

(defun evalator-elisp-transform-candidates (cands expr-str collect-p)
  "See slot documentation in evalator-context.el."
  (let ((cands-xfrmd (if collect-p
                         (list (evalator-elisp-eval
                                (evalator-elisp-subst-special-args
                                 expr-str (mapcar 'read cands))))
                       (mapcar (lambda (c)
                                 (evalator-elisp-eval
                                  (evalator-elisp-subst-special-args
                                   expr-str (read c)))) cands))))
    (mapcar `prin1-to-string cands-xfrmd)))

(provide 'evalator-elisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator-elisp.el ends here
