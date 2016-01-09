(require 'helm-lambda-context)
(require 'eieio)

(defun helm-lambda-context-elisp-make-candidates (input &optional skip-readp)
  (let* ((data (if skip-readp input (eval (read input))))
         (to-obj-string (lambda (x)
                          (prin1-to-string x))))
    (cond
     ((stringp data) (list data))
     ((sequencep data) (mapcar to-obj-string data))
     (t (list (funcall to-obj-string data))))))

(defun helm-lambda-context-elisp-transform-candidates-try (candidates-all candidates-marked expression)
  (condition-case err
      (helm-lambda-context-elisp-transform-candidates candidates-all
                                                      candidates-marked
                                                      expression)
    (error
     ;; TODO Would be useful to have a red/green flash for this
     candidates-all)))

(defun helm-lambda-context-elisp-transform-candidates (candidates-all candidates-marked expression)
  (helm-lambda-context-elisp-make-candidates
   (if (equal nil candidates-marked)
       (mapcar (lambda (candidate)
                 (helm-lambda-context-elisp-apply-expression expression
                                                             candidate))
               candidates-all)
     (helm-lambda-context-elisp-apply-expression expression
                                                 candidates-marked))
   t))

(defun helm-lambda-context-elisp-apply-expression (expression-str x)
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
(setq helm-lambda-context-elisp
      (make-instance
       'helm-lambda-context
       
       :name
       "ELisp"

       :init
       (lambda () nil)

       :make-candidates
       'helm-lambda-context-elisp-make-candidates
       

       :transform-candidates-try
       'helm-lambda-context-elisp-transform-candidates-try
       

       :transform-candidates
       'helm-lambda-context-elisp-transform-candidates))

(provide 'helm-lambda-context-elisp)
