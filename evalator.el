(require 'cl-lib)
(require 'evalator-context)
(require 'evalator-faces)
(require 'evalator-history)
(require 'evalator-key-map)
(require 'evalator-state)
(require 'helm)

;; TODO There's currently a weird bug happening where spamming the history next
;; and previous actions will cause the evalator session to shut down. Has to do with
;; let bindings being nested too deep.
(defun evalator-action-previous ()
  "Go to the next history state and update the evalator session."
  (interactive)
  (when (not (equal 0 (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ -1 (evalator-history-index)))
    (evalator-load)))

(defun evalator-action-next ()
  "Go to the previous history state and update the evalator session."
  (interactive)
  (when (not (equal (+ -1 (length (evalator-history))) (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index)))
    (evalator-load)))

(defun evalator-action-confirm ()
  "Accepts results and starts a new evalator for further
transformation."
  (interactive)
  (let* ((err-handler (lambda ()
                        (message "Can't update, invalid expression")
                        nil))
         (candidates (evalator-transform-candidates
                      helm-pattern
                      (plist-get evalator-state :mode)
                      (slot-value (plist-get evalator-state :context) :make-candidates)
                      (slot-value (plist-get evalator-state :context) :transform-candidates)
                      err-handler))
         (f (lambda (candidates _) (evalator :candidates candidates
                                             :initp      nil
                                             :hist-pushp t))))
    (when candidates
      (helm-exit-and-execute-action (apply-partially f candidates)))))

(defun evalator-action-insert-arg ()
  "Inserts the special evalator arg into the expression prompt"
  (interactive)
  ;; Need to get this from context instead
  (insert evalator-context-special-arg-default))

(defun evalator-load ()
  "Quits the current evalator session and loads a new one."
  (let* ((source (evalator-history-current :source))
         (candidates (helm-get-candidates source))
         (f (lambda (candidates _) (evalator :candidates candidates
                                             :initp      nil
                                             :hist-pushp nil))))
    (helm-exit-and-execute-action (apply-partially f candidates))))

(defun evalator-flash (status)
  "Changes the expression prompt face to 'evalator-(success | error)'
depending on the 'status' arg"
  (let ((f (if (equal :success status) 'evalator-success 'evalator-error)))
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (face-remap-add-relative 'minibuffer-prompt f))))

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

(defun evalator-persistent-help ()
  "Builds persistent help string"
  (cl-flet ((f (command)
               (key-description (where-is-internal command evalator-key-map t))))
    (concat "History forward, "
            (f 'evalator-action-previous)    ": History backward, "
            (f 'evalator-action-confirm)     ": Accept transformation, "
            (f 'evalator-action-insert-arg)  ": Insert special arg")))

(defun evalator-build-source (candidates mode)
  "Builds the source for a evalator session.  Accepts a list of
candidates."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :candidates candidates
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      (with-helm-current-buffer
                                        (evalator-transform-candidates
                                         helm-pattern
                                         (plist-get evalator-state :mode)
                                         (slot-value (plist-get evalator-state :context) :make-candidates)
                                         (slot-value (plist-get evalator-state :context) :transform-candidates))))
    :keymap evalator-key-map
    :nohighlight t
    :nomark (equal :explicit mode)
    :persistent-help (evalator-persistent-help)
    :volatile t))

(defun evalator-transform-candidates (expr mode make-f transform-f &optional err-handler)
  ""
  (let ((cands-all (helm-get-candidates (helm-get-current-source)))
        (cands-marked (evalator-marked-candidates)))
    (condition-case err
        (progn (evalator-flash :success)
               (if (equal 0 (evalator-history-index))
                   (progn (funcall make-f expr mode))
                 (funcall transform-f cands-all cands-marked expr mode)))
      (error
       (if err-handler
           (funcall err-handler)
         (progn
           (evalator-flash :error)
           cands-all))))))

(defun evalator-insert-last-equiv-expr ()
  "Inserts the equivalent expression of the previous evalator
session.  NOTE: Session must have been run with 'evalator-explicit'
for this to work."
  (interactive)
  (let* ((exprs (cdr (mapcar
                      (lambda (h)
                        (plist-get h :expression))
                      (plist-get evalator-state :history)))))
    (with-current-buffer
        (insert (reduce
                 (lambda (e1 e2)
                   (replace-regexp-in-string evalator-context-special-arg-default e1 e2 t))
                 exprs)))))

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
      (evalator-utils-put! evalator-state :mode (plist-get o :mode)))
    (funcall (slot-value (plist-get evalator-state :context) :init)))
  
  (let* ((helm-mode-line-string "")
         (candidatesp (not (equal nil (plist-get o :candidates))))
         (candidates (if candidatesp
                         (plist-get o :candidates)
                       '("Enter an expression below to generate initial data")))
         (source (evalator-build-source candidates (plist-get evalator-state :mode))))
    
    (when (or (called-interactively-p 'any) (plist-get o :hist-pushp))
      (evalator-history-push! source helm-pattern))
    
    (helm :sources source
          :buffer "*evalator*"
          :prompt "Expression: ")))

(defun evalator-explicit ()
  (interactive)
  (evalator :initp      t
            :hist-pushp t
            :mode       :explicit))

(provide 'evalator)

;; Dev
;; TODO comment or remove these when development done
;; (defun evalator-dev-reload-elisp ()
;;   (interactive)
;;   (let ((contextel "evalator-context.el")
;;         (elispel "evalator-context-elisp.el")
;;         (evalatorel "evalator.el"))
;;     (with-current-buffer contextel
;;       (save-buffer)
;;       (eval-buffer))
;;     (with-current-buffer elispel
;;       (save-buffer)
;;       (eval-buffer))
;;     (with-current-buffer evalatorel
;;       (save-buffer)
;;       (eval-buffer))
;;     (setq evalator-state (plist-put evalator-state :context evalator-context-elisp))))

;; (defun evalator-dev-reload-cider ()
;;   (interactive)
;;   (let ((ciderclj "evalator-context-cider.clj")
;;         (ciderel "evalator-context-cider.el")
;;         (testclj "test.clj")
;;         (lambdael "evalator.el"))
;;     (with-current-buffer ciderclj
;;       (save-buffer)
;;       (cider-eval-buffer))
;;     (with-current-buffer testclj
;;       (save-buffer)
;;       (cider-eval-buffer))
;;     (with-current-buffer ciderel
;;       (save-buffer)
;;       (eval-buffer))
;;     (setq evalator-state (plist-put evalator-state :context evalator-context-cider))))

;; (defun evalator-dev ()
;;   (interactive)
;;   (evalator-dev-reload)
;;   (evalator :initp t :hist-pushp t))

