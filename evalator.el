(require 'cl-lib)
(require 'evalator-context)
(require 'evalator-faces)
(require 'evalator-history)
(require 'evalator-action)
(require 'evalator-key-map)
(require 'evalator-state)
(require 'helm)

(defun evalator-flash (status)
  (let ((f (if (equal :success status) 'evalator-success 'evalator-error)))
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (face-remap-add-relative 'minibuffer-prompt f))))

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

(defun evalator-persistent-help ()
  (cl-flet ((f (command)
            (key-description (where-is-internal command evalator-key-map t))))
    (concat "History forward, "
            (f 'evalator-action-previous)    ": History backward, "
            (f 'evalator-action-confirm) ": Accept transformation, "
            (f 'evalator-action-insert)  ": Insert special arg")))

(defun evalator-build-source (candidates mode)
  "Builds the source for a evalator session.  Accepts a list of
candidates."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :candidates candidates
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      (with-helm-current-buffer
                                        (evalator-transform-candidates)))
    :keymap evalator-key-map
    :nohighlight t
    :nomark (equal :explicit mode)
    :persistent-help (evalator-persistent-help)
    :volatile t))

(defun evalator-transform-candidates (&optional err-handler)
  "Call the evaluation contexts transform candidates function.  If the
should-try flag is non-nil then the 'transform-candidates-try' flag is
called.  Otherwise, the 'transform-candidates function is called."
  (let* ((candidates-all (helm-get-candidates (evalator-history-current :source)))
         (make-candidates (slot-value (plist-get evalator-state :context) :make-candidates))
         (transform (slot-value (plist-get evalator-state :context) :transform-candidates)))
    (condition-case err
        (progn (evalator-flash :success)
               (if (equal 0 (evalator-history-index))
                   (progn (setq evalator-state (plist-put evalator-state :seed helm-pattern))
                          (funcall
                           make-candidates
                           helm-pattern
                           (plist-get evalator-state :mode)))
                 (funcall
                  transform
                  candidates-all
                  (evalator-marked-candidates)
                  helm-pattern
                  (plist-get evalator-state :mode))))
      (error
       (if err-handler
           (funcall err-handler)
         (progn
           (evalator-flash :error)
           candidates-all))))))

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
      (setq evalator-state (plist-put evalator-state :mode (plist-get o :mode))))
    (funcall (slot-value (plist-get evalator-state :context) :init)))
  
  (let* ((helm-mode-line-string "")
         (candidatesp (not (equal nil (plist-get o :candidates))))
         (candidates (if candidatesp
                         (plist-get o :candidates)
                       '("Enter an expression below to generate initial data")))
         (source (evalator-build-source candidates (plist-get evalator-state :mode))))
    
    (when (or (called-interactively-p 'any) (plist-get o :hist-pushp))
      (evalator-history-push! source))
    
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

