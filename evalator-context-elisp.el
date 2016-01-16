(require 'cl-lib)
(require 'eieio)
(require 'evalator-context)

(defvar evalator-context-elisp-special-arg nil)

(defun evalator-context-elisp-make-equiv-expr (exprs)
  "Accepts a list of expressions, EXPRS, and returns the equivalent
expression by substituting any special args with the expression
before it.

Example: 

(evalator-context-elisp-make-equiv-expr '((1 2 3 4) (cons 0 Ⓔ))) 
=> (cons 0 '(1 2 3 4))"
  (let* ((spec-arg (evalator-context-get-special-arg evalator-context-elisp))
         (sub (lambda (e1 e2)
                (replace-regexp-in-string spec-arg e1 e2 t))))
    (reduce sub exprs)))

(defun evalator-context-elisp-substitute-special-args (expr c)
  "Walks through the expression, EXPR, and replaces any special args with
the candidate C or the element within C depending on the special arg.

Example:

(evalator-context-elisp-substitute-special-args '(+ 1 Ⓔ) 1)
=> '(+ 1 1)

(evalator-context-elisp-substitute-special-args '(+ 1 Ⓔ1) [0 1 2])
=> '(+ 1 1)
"
  (let* ((spec-arg (evalator-context-get-special-arg evalator-context-elisp))
         (regex (format "%s[0-9]*" spec-arg)))
    (cl-labels ((quote-if-list (x)
                               (if (consp x) `'(,@x) x))
                
                (get-elt (sym)
                         (quote-if-list (elt c (string-to-number (subseq (symbol-name sym) 1)))))
                
                (subst (x)
                       (if (symbolp x)
                           (cond ((equal (symbol-name x) spec-arg)
                                  (quote-if-list c))
                                 
                                 ((string-match regex (symbol-name x))
                                  (quote-if-list (get-elt x)))
                                
                                 (t x))
                         x))
                
                (walk (expr)
                     (cond ((symbolp expr) (subst expr)) 
                           ((equal nil (car expr)) nil)
                           ((consp (car expr)) (cons (walk (car expr)) (walk (cdr expr))))
                           (t (cons (subst (car expr)) (walk (cdr expr)))))))
      (walk expr))))

(defun evalator-context-elisp-make-candidates (input mode &optional not-initial-p)
  "Converts INPUT into a valid list of helm candidates.  In other
words, a list of the stringified representation of the input.  How
INPUT is converted depends on both the MODE argument and the optional
NOT-INITIAL-P flag.  If NOT-INITIAL-P is nil then it is assumed that
INPUT came from user input and first needs to be read and evaluated to
an elisp object.  If NOT-INITIAL-P is non-nil then it is treated as an
elisp object.  If MODE is :explicit then the function will always
return a candidate list of one element.  If MODE is some other value
then the function will return a candidate list equivalent to the size
of the input object.  That means scalars will be returned in a size 1
candidates list.  Vectors and lists will be returned in a candidates
list whose size is equal to the size of the collection."
  (let* ((data (if not-initial-p input (eval (read input))))
         (to-obj-string (lambda (x)
                          (prin1-to-string x))))
    (cond
     ((equal :explicit mode) (if not-initial-p
                                 (list (funcall to-obj-string (car data)))
                               (list (funcall to-obj-string data))))
     ((and (not (stringp data)) (sequencep data)) (mapcar to-obj-string data))
     (t (list (funcall to-obj-string data))))))

(defun evalator-context-elisp-transform-candidates (candidates-all candidates-marked expr-str mode)
  "Reads the input expression string, EXPR-STR, uses that to transform
either the entire candidates list, CANDIDATES-ALL, or the marked
candidates, CANDIDATES-MARKED.  If CANDIDATES-MARKED is nil then
expression is mapped over CANDIDATES-ALL.  Otherwise, the expression
is applied to the the entire CANDIDATES-MARKED list.  The function
returns a valid helm candidate list of of the result of the operation
above."
  (evalator-context-elisp-make-candidates
   (if (equal nil candidates-marked)
       (mapcar (lambda (candidate)
                 (evalator-context-elisp-eval (read expr-str)
                                              (read candidate)))
               candidates-all)
     (evalator-context-elisp-eval (read expr-str)
                                  (mapcar 'read candidates-marked)))
   mode
   t))

(defun evalator-context-elisp-eval (expr x)
  "Substitutes any special args in the expression, EXPR, with X(or the
element within X) and evaluates the expression."
  (let ((new-expr (evalator-context-elisp-substitute-special-args expr x)))
    (eval new-expr)))

;; TODO kinda confused on the difference between defvar and setq, figure this out.
(defvar evalator-context-elisp
  (make-instance
   'evalator-context
   
   :name
   "ELisp"

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

(provide 'evalator-context-elisp)
