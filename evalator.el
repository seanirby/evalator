(require 'helm)
(require 'cl-lib)
(require 'evalator-context)
(require 'evalator-context-elisp)

;; State

(defvar evalator-state-default (list :context       evalator-context-elisp
                                     :mode          :normal
                                     :seed          nil
                                     :history       []
                                     :history-index -1))

(defvar evalator-state (copy-sequence evalator-state-default))
(defvar evalator-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "RET") 'evalator-confirm-application)
    (define-key map (kbd "C-u") 'evalator-confirm-application)
    (define-key map (kbd "C-j") 'evalator-history-next)
    (define-key map (kbd "C-l") 'evalator-history-previous)
    (define-key map (kbd "C-i") 'evalator-insert-special-arg)
    map))

(defun evalator-state-init ()
  "Set state back to initial value."
 (setq evalator-state (copy-sequence evalator-state-default)))




;; History

(defun evalator-history ()
  "Return history"
  (plist-get evalator-state :history))

(defun evalator-history-index ()
  "Return current history index"
  (plist-get evalator-state :history-index))

(defun evalator-history-push! (data)
  "Push the current source and expression onto history"
  (setq evalator-state (plist-put evalator-state :history
                                  (vconcat (subseq (evalator-history) 0 (+ 1 (evalator-history-index)))
                                           (list (list :source data :expression helm-pattern)))))
  (setq evalator-state (plist-put evalator-state :history-index (+ 1 (evalator-history-index)))))

(defun evalator-history-current (&optional k)
  "Retrieve active history element.  Accepts an optional key."
  (let ((h (elt (evalator-history) (evalator-history-index))))
    (if k (plist-get h k) h)))

(defun evalator-history-load ()
  "Quits the current evalator session and loads a new one."
  (let* ((source (evalator-history-current :source))
         (candidates (helm-get-candidates source))
         (f (lambda (candidates _) (evalator :candidates candidates
                                             :initp      nil
                                             :hist-pushp nil))))
    (helm-exit-and-execute-action (apply-partially f candidates))))




;; Key Actions

;; TODO There's currently a weird bug happening where spamming the history next
;; and previous actions will cause the evalator session to shut down. Has to do with
;; let bindings being nested too deep.
(defun evalator-history-previous ()
  "Go to the next history state and update the evalator session."
  (interactive)
  (when (not (equal 0 (evalator-history-index)))
    (setq evalator-state (plist-put evalator-state :history-index (+ -1 (evalator-history-index))))
    (evalator-history-load)))

(defun evalator-history-next ()
  "Go to the previous history state and update the evalator session."
  (interactive)
  (when (not (equal (+ -1 (length (evalator-history))) (evalator-history-index)))
    (setq evalator-state (plist-put evalator-state :history-index (+ 1 (evalator-history-index))))
    (evalator-history-load)))

(defun evalator-confirm-application ()
  "Accepts results and starts a new evalator for further
transformation."
  (interactive)
  (let ((source (evalator-history-current :source))
        (candidates (evalator-transform-candidates nil))
        (f (lambda (candidates _) (evalator :candidates candidates
                                            :initp      nil
                                            :hist-pushp t))))
    (helm-exit-and-execute-action (apply-partially f candidates))))

(defun evalator-insert-special-arg ()
  (interactive)
  ;; Need to get this from context instead
  (insert evalator-context-special-arg-default))





;; Other

(defun evalator-get-input ()
  (read-from-minibuffer "Enter initial data:"))

(defun evalator-thing-before-point (&optional limits regexp)
  "TEMP"
  (save-excursion
    (buffer-substring-no-properties (point-at-bol) (point))))

(cl-defun evalator-marked-candidates (&key with-wildcard)
  "Same as 'helm-marked-candidates' except it returns nil 
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

(defun evalator-build-source (candidates mode)
  "Builds the source for a evalator session.  Accepts a list of
candidates."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :volatile t
    :candidates candidates
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      ;; TODO might be possible to move condition-case to this level
                                      (with-helm-current-buffer
                                        (evalator-transform-candidates t)))
    :keymap evalator-map
    :nohighlight t
    :nomark (equal :explicit mode)))





;; Context evaluation

(defun evalator-transform-candidates (should-try)
  "Call the evaluation contexts transform candidates function.  If the
should-try flag is non-nil then the 'transform-candidates-try' flag is
called.  Otherwise, the 'transform-candidates function is called."
  (let* ((keyword (if should-try :transform-candidates-try :transform-candidates))
         (transform-func (slot-value (plist-get evalator-state :context) keyword)))
    (funcall
     transform-func
     (helm-get-candidates (evalator-history-current :source))
     (evalator-marked-candidates)
     helm-pattern
     (plist-get evalator-state :mode))))





;; User functions

(defun evalator-insert-last-equiv-expr ()
  "Inserts the equivalent expression of the previous evalator
session.  NOTE: Session must have been run with 'evalator-explicit'
for this to work."
  (interactive)
  (let ((exprs (cons (plist-get evalator-state :seed)
                     (cdr (mapcar (lambda (h)
                                    (plist-get h :expression)) (plist-get evalator-state :history))))))
    (with-current-buffer
        (insert (reduce (lambda (e1 e2) (replace-regexp-in-string "%" e1 e2)) exprs)))))

(defun evalator-resume ()
  "Resumes last evalator session."
  (interactive)
  (helm-resume "*evalator*"))

(defun evalator (&rest o)
  "Starts a helm session for interactive evaluation and transformation
of input data.  Optional argument o may have the following properties:

:input 
The input string used to make the helm candidates.  Will be the
text behind the cursor if not present.

:candidates 
The candidates to use for building a helm source. If this
argument is present the :input property is ignored.

:initp 
Flag to force initialization.  Initialization will always occur if
evalator is called interactively.

:hist-pushp 
Flag to force push source onto history.  A history push will always
occur if evalator is called interactively.

:mode 
Tells helm lambda what mode to use.  Defaults to :normal."
  (interactive)
  
  (when (or (called-interactively-p 'any) (plist-get o :initp))
    (evalator-state-init)
    (when (plist-get o :mode)
      (setq evalator-state (plist-put evalator-state :mode (plist-get o :mode))))
    (funcall (slot-value (plist-get evalator-state :context) :init)))

  (let* ((candidatesp (not (equal nil (plist-get o :candidates))))
         (input (when (not candidatesp)
                  (or (plist-get o :input) (evalator-get-input) t)))
         (candidates (if candidatesp
                         (plist-get o :candidates)
                       (funcall (slot-value (plist-get evalator-state :context) :make-candidates)
                                input
                                (plist-get evalator-state :mode))))
         (source (evalator-build-source candidates (plist-get evalator-state :mode))))
    
    ;; Remember initial input so an equivalent expression can be created later
    (when (or (called-interactively-p 'any) (plist-get o :initp))
      (setq evalator-state (plist-put evalator-state :seed input)))
    
    (when (or (called-interactively-p 'any) (plist-get o :hist-pushp))
      (evalator-history-push! source))
    
    (helm :sources source
          :buffer "*evalator*"
          :prompt "Expression: ")))

(defun evalator-explicit ()
  (interactive)
  (evalator :input      (evalator-get-input)
            :initp      t
            :hist-pushp t
            :mode       :explicit))

(provide 'evalator)

;; Dev
;; TODO comment or remove these when development done
(defun evalator-dev-reload-elisp ()
  (interactive)
  (let (
        (elispel "evalator-context-elisp.el")
        (evalatorel "evalator.el"))
    (with-current-buffer elispel
      (save-buffer)
      (eval-buffer))
    (with-current-buffer evalatorel
      (save-buffer)
      (eval-buffer))
    (setq evalator-state (plist-put evalator-state :context evalator-context-elisp))))

(defun evalator-dev-reload-cider ()
  (interactive)
  (let ((ciderclj "evalator-context-cider.clj")
        (ciderel "evalator-context-cider.el")
        (testclj "test.clj")
        (lambdael "evalator.el"))
    (with-current-buffer ciderclj
      (save-buffer)
      (cider-eval-buffer))
    (with-current-buffer testclj
      (save-buffer)
      (cider-eval-buffer))
    (with-current-buffer ciderel
      (save-buffer)
      (eval-buffer))
    (setq evalator-state (plist-put evalator-state :context evalator-context-cider))))

(defun evalator-dev ()
  (interactive)
  (evalator-dev-reload)
  (evalator :initp t :hist-pushp t))

