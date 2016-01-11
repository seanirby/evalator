(require 'evalator-context)
(require 'eieio)

(defun evalator-context-elisp-substitute-special-args (expr x)
  "Walks through the form expr and replaces any special args with their proper value."
  (cl-labels ((quote-if-list (x)
                             (if (consp x) (quote x) x))
              (get-elt (sym)
                       (quote-if-list (elt x (string-to-number (subseq (symbol-name sym) 1)))))
              (subst (sym)
                     (cond ((equal sym '%)
                            (quote-if-list x))
                           
                           ((and (symbolp sym)
                                 (string-match "%[0-9]*" (symbol-name sym)))
                            (quote-if-list (get-elt sym)))
                           
                           (t
                            sym)))
              (rec (xs)
                   (cond ((equal nil (car xs)) nil)
                         ((consp (car xs)) (cons (rec (car xs)) (rec (cdr xs))))
                         (t (cons (subst (car xs)) (rec (cdr xs)))))))
    (rec expr)))

(defun evalator-context-elisp-make-candidates (input mode &optional not-initialp)
  (let* ((data (if not-initialp input (eval (read input))))
         (to-obj-string (lambda (x)
                          (prin1-to-string x))))
    (my-sp (funcall to-obj-string data))
    (cond
     ((equal :explicit mode) (if not-initialp
                                 (list (funcall to-obj-string (car data)))
                               (list (funcall to-obj-string data))))
     ((and (not (stringp data)) (sequencep data)) (mapcar to-obj-string data))
     (t (list (funcall to-obj-string data))))))

(defun evalator-context-elisp-transform-candidates-try (candidates-all candidates-marked expr-str mode)
  (condition-case err
      (evalator-context-elisp-transform-candidates candidates-all
                                                   candidates-marked
                                                   expr-str
                                                   mode)
    (error
     ;; TODO Would be useful to have a red/green flash for this
     candidates-all)))

(defun evalator-context-elisp-transform-candidates (candidates-all candidates-marked expr-str mode)
  (evalator-context-elisp-make-candidates
   (if (equal nil candidates-marked)
       (mapcar (lambda (candidate)
                 (evalator-context-elisp-apply-expression (read expr-str)
                                                          (read candidate)))
               candidates-all)
     (evalator-context-elisp-apply-expression (read expr-str)
                                              (mapcar 'read candidates-marked)))
   mode
   t))

(defun evalator-context-elisp-apply-expression (expr x)
  (let ((new-expr (evalator-context-elisp-substitute-special-args expr x)))
    (my-sp new-expr)
    (eval new-expr)))

(defun evalator-context-elisp-apply-expression2 (expression-str x)
  (let ((expression (read expression-str)))
    (if (and (sequencep x) (not (stringp x)))
        (let* ((xs (mapcar 'read x))
               (ns (number-sequence 0 (1- (length xs))))
               (arg-names (mapcar (lambda (n) (intern (concat "%" (int-to-string n)))) ns))
               (% xs)
               (f `(lambda ,arg-names ,expression)))
          (apply (eval f) (append xs nil)))
      (let* ((% (read x)))
        (eval expression)))))

;; TODO kinda confused on the difference between defvar and setq, figure this out.
(setq evalator-context-elisp
      (make-instance
       'evalator-context
       
       :name
       "ELisp"

       :init
       (lambda () nil)

       :make-candidates
       'evalator-context-elisp-make-candidates
       

       :transform-candidates-try
       'evalator-context-elisp-transform-candidates-try
       

       :transform-candidates
       'evalator-context-elisp-transform-candidates))

(provide 'evalator-context-elisp)
