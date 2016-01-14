(require 'cl-lib)
(require 'evalator-context)
(require 'evalator-faces)
(require 'evalator-history)
(require 'evalator-key-map)
(require 'evalator-state)
(require 'helm)

(defvar evalator-candidates-initial '("Enter an expression below to generate initial data"))

(defvar evalator-prompt-f
  (lambda (hist-ind hist-length)
    (let ((hist (format "%s of %s" (+ 1 hist-ind) hist-length))
          (sep "|")
          (expr-prompt "Expression:" ))
      (mapconcat 'identity `(,hist ,sep ,expr-prompt) " ")))
  "Points to a function that is called with the current history index
and length.  Will be used to generate the evalator prompt") 

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

(defun evalator-action-insert-special-arg ()
  "Inserts the special evalator arg into the expression prompt"
  (interactive)
  (insert (evalator-context-get-special-arg (plist-get evalator-state :context))))

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
            (f 'evalator-action-previous)            ": History backward, "
            (f 'evalator-action-confirm)             ": Accept transformation, "
            (f 'evalator-action-insert-special-arg)  ": Insert special arg")))

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

(defun evalator-insert-last-equiv-expr (&optional exprs)
  "Inserts the equivalent expression of the previous evalator
session.  NOTE: Session must have been run with 'evalator-explicit'
for this to work."
  (interactive)
  (let* ((spec-arg (evalator-context-get-special-arg (plist-get evalator-state :context)))
         (sub (lambda (e1 e2)
               (replace-regexp-in-string spec-arg e1 e2 t)))
        (expr-chain (if exprs exprs (evalator-history-expression-chain))))
    (insert (reduce sub expr-chain))))

(defun evalator-resume ()
  "Resumes last evalator session."
  (interactive)
  (helm-resume "*evalator*"))

(defun evalator-init-history (source expr pushp)
  "Pushes source and expr onto history if necessary."
  (when pushp
    (evalator-history-push! source expr)))

(defun evalator-init (init-f initp mode)
  "Calls context initialization function and sets the mode if
necessary."
  (when initp
    (evalator-state-init)
    (when mode
      (evalator-utils-put! evalator-state :mode mode))
    (funcall init-f)))

(defun evalator (&rest opts)
  "Starts a helm session for interactive evaluation and transformation
of inp"
  (interactive)

  (evalator-init (slot-value (plist-get evalator-state :context) :init)
                 (or (called-interactively-p 'any)
                     (plist-get opts :initp))
                 (plist-get opts :mode))

  (let* ((helm-mode-line-string "")
         (candidatesp (not (equal nil (plist-get opts :candidates))))
         (candidates (if candidatesp
                         (plist-get opts :candidates)
                       evalator-candidates-initial))
         (source (evalator-build-source candidates (plist-get evalator-state :mode))))
    
    (evalator-init-history source
                           helm-pattern
                           (or (called-interactively-p 'any)
                               (plist-get opts :hist-pushp)))

    (helm :sources source
          :buffer "*evalator*"
          :prompt (funcall evalator-prompt-f
                           (evalator-history-index)
                           (length (evalator-history))))))

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

