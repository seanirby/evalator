(require 'helm)
(require 'helm-elisp)
(require 'cl-lib)

(setq helm-lambda-history '())
(defvar helm-lambda-history '())
(defvar helm-lambda-eval-context nil) ;;stub
(defvar helm-lambda-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-u") 'helm-lambda-confirm-application)
    (define-key map (kbd "C-j") 'helm-lambda-history-next)
    (define-key map (kbd "C-l") 'helm-lambda-history-previous)
    map))

(cl-defun -helm-lambda-marked-candidates (&key with-wildcard)
  "Same as 'helm-marked-candidates' except it returns an empty list
if no candidates were marked."
  (with-current-buffer helm-buffer
    (let ((candidates
           (cl-loop with current-src = (helm-get-current-source)
                    for (source . real) in (reverse helm-marked-candidates)
                    when (equal (assq 'name source) (assq 'name current-src))
                    append (helm--compute-marked real source with-wildcard) 
                    into cands
                    finally return cands)))
      candidates)))

(defun helm-lambda-history-push! (data)
  (setq helm-lambda-history (cons data helm-lambda-history)))

(defun helm-lambda-history-pop! ()
  (setq helm-lambda-history (cdr helm-lambda-history)))

(defun helm-lambda-history-clear! ()
  (setq helm-lambda-history '()))

(defun helm-lambda-history-current ()
  (car helm-lambda-history))

(defun helm-lambda-history-previous ()
  (interactive)
  (message-box "going to previous source"))

(defun helm-lambda-history-next ()
  (interactive)
  (message-box "going to next source"))

(defun helm-lambda-confirm-application ()
  "Accepts results and starts a new helm-lambda for further
transformations."
  (interactive)
  (let ((source (helm-lambda-history-current))
	(candidates (-helm-lambda-transform-candidates))
	(f (lambda (_ candidates) (helm-lambda candidates))))
    (helm-exit-and-execute-action (apply-partially f candidates))))

(defun -helm-lambda-transform-candidates ()
  "Transform a group of candidates with the expression entered in the
minibuffer.  If the user hasn't marked any candidates in the helm
session then the expression is mapped over all the candidates. If
there are marked candidates then the expression is applied to the list
of marked candidates.  The former is useful for candidate
transformations.  The latter is useful for accumulating a result."
  (let ((expr (read helm-pattern))
	(candidates (helm-get-candidates (helm-lambda-history-current))))
    (my-sp expr)
    (my-sp "candidates from history")
    (my-sp candidates)

    ;; TODO will need fix for marked candidates
    (if (equal nil (-helm-lambda-marked-candidates))
	(mapcar (lambda (candidate)
		  (prin1-to-string (-helm-lambda-apply-expression expr (read candidate))))
		candidates)
      ;; TODO will need fix for marked candidates
      (list (prin1-to-string (-helm-lambda-apply-expression expr (mapcar 'read (helm-marked-candidates))))))))

(defun -helm-lambda-transform-candidates-try (_candidates _source)
  "Attempts -helm-lambda-transform-candidates"
  (condition-case err
      (with-helm-current-buffer
	(-helm-lambda-transform-candidates))
    (error
     ;; TODO would be useful to have a red/green flash for this
     (helm-get-candidates _source))))

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
    :candidates data
    :filtered-candidate-transformer #'-helm-lambda-transform-candidates-try
    :keymap helm-lambda-map
    :nohighlight t))

(defun helm-lambda-raw (data)
  "Starts a helm session for interactive evaluation and transformation
of input data."
  (let ((source (-helm-lambda-build-evaluation-result-source data)))
    (helm-lambda-history-push! source)
    (helm :sources source
	  :prompt "Expression: ")))

(defun helm-lambda-stringify-listify (data)
  "Converts input data to a list of stringified lisp objects before
passing it to helm-lambda-raw.  Intended to be the entry point to a
helm-lambda session."
  (let* ((to-obj-string (lambda (x)
			  (prin1-to-string x)))
	 ;;Convert data to a list if not already.
	 ;;Convert all elements in list to the string
	 ;;representation of its lisp object.
	 (data-stringified   (if (and (sequencep data) (not (stringp data)))
				 (mapcar to-obj-string data)
			       (list (funcall to-obj-string data)))))
    (helm-lambda-raw data-stringified)))

(defalias 'helm-lambda 'helm-lambda-stringify-listify
  "Starts a helm session for interactive evaluation and transformation
  of input data.")

