(require 'helm)
(require 'helm-elisp)
(require 'cl-lib)

(defvar helm-lambda-history '())
(defvar helm-lambda-eval-context nil) ;;stub

;; History
;; TODO: Should define a history limit
(defun helm-lambda-history-push! (data)
  (setq helm-lambda-history (cons data helm-lambda-history)))

(defun helm-lambda-history-pop! ()
  (setq helm-lambda-history (cdr helm-lambda-history)))

(defun helm-lambda-history-clear! ()
  (setq helm-lambda-history '()))

(defun helm-lambda-current-data ()
  (car helm-lambda-history))

;; Might need this later for looking up emacs commands and functions
(setq helm-lambda-emacs-commands-and-functions
      (let ((sources `(,(helm-def-source--emacs-functions)
		       ,(helm-def-source--emacs-commands)))
	    (partial (lambda (name f index)
		       `(lambda (src)
			  (helm-add-action-to-source ,name (quote ,f) src ,index)
			  src))))
	(mapcar (funcall partial "Identity" 'identity 0) sources)))

(defun -helm-lambda-apply-expression (expr x)
  "Applies 'expr' to 'x'. Provides
wrapper such that expression can
include '%' symbols. '%' will
refer to x. If 'x' is a sequence
then elements in x can be
referenced with %INDEX.

Example:

(setq exp '(cons (+ %0 %1 %2 %3) %))
(setq x '(1 2 3 4)
(helm-lambda-apply-expression exp x)  => '(10 1 2 3 4)"
  (if (and (sequencep x) (not (stringp x)))
      (let* ((ns (number-sequence 0 (1- (length x))))
	     (arg-names (mapcar (lambda (n) (intern (concat "%" (int-to-string n)))) ns))
	     (% x)
	     (f `(lambda ,arg-names ,expr)))
	(apply (eval f) (append x nil)))
    (let* ((% x))
      (eval expr))))

(defun -helm-lambda-build-evaluation-result-source (data)
  (helm-build-sync-source "Evaluation Result"
    :volatile t
    :candidates (helm-lambda-current-data)
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      (condition-case err
					  (with-helm-current-buffer
					    (let ((expr (read helm-pattern)))
					      (mapcar (lambda (candidate)
							(let ((c-obj (read candidate)))
							  (prin1-to-string (-helm-lambda-apply-expression expr c-obj))
							  ))
						      (helm-lambda-current-data))))
					(error
					 (helm-lambda-current-data))))))

(defun helm-lambda (data)
  "Starts a helm buffer for
interactive evaluation. Input text
will be evaluated according to the
context set in the
'helm-lambda-eval-context'
variable."
  (let* ((to-obj-string (lambda (x)
			  (prin1-to-string x)))
	 ;;Convert data to a list if not already.
	 ;;Convert all elements in list to the string
	 ;;representation of its lisp object.
	 (data-coerced   (if (and (sequencep data) (not (stringp data)))
			     (mapcar to-obj-string data)
			   (list (funcall to-obj-string data)))))
    (helm-lambda-history-push! data-coerced)
    (helm :sources (-helm-lambda-build-evaluation-result-source data-coerced))))


