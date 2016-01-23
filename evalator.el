;;; evalator.el --- Emacs package for interactive transformation of data
;; 
;; Copyright © , Sean Irby
;; Author: Sean Irby
;; Maintainer: Sean Irby <sean.t.irby@gmail.com>
;; URL: http://www.github.com/seanirby/evalator
;; Version: 0.0.1
;; Keywords: languages, elisp, helm
;; Package-Requires: ((helm-core "1.9.1"))
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
(require 'evalator-context)
(require 'evalator-context-elisp)
(require 'evalator-faces)
(require 'evalator-history)
(require 'evalator-key-map)
(require 'evalator-state)
(require 'evalator-utils)
(require 'helm)

(defvar evalator-candidates-initial '("Enter an expression below to generate initial data")
  "Informational candidate used on evalator startup.")
(defvar evalator-error nil
  "Error symbol used for signaling evalator errors.")
(put 'evalator-error 'error-conditions '(error))

(defvar evalator-prompt-f
  (lambda ()
    (format "%s of %s" (+ 1 (evalator-history-index)) (length (evalator-history))))
  "Var's value is a function used to generate the evalator prompt.")

(defun evalator-action-previous ()
  "Go to the next history state and update the evalator session."
  (interactive)
  (when (not (equal 0 (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ -1 (evalator-history-index)))
    (helm-unmark-all)
    (helm-set-pattern "")
    (helm-update)))

(defun evalator-action-next ()
  "Go to the previous history state and update the evalator session."
  (interactive)
  (when (not (equal (+ -1 (length (evalator-history))) (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index)))
    (helm-unmark-all)
    (helm-set-pattern "")
    (helm-update)))

(defun evalator-action-execute-in-elisp ()
  "Execute expression in elisp context.
This function is useful if you want to execute an Emacs command or
Elisp function from within an evalator session that uses a different
evaluation context.  This action does not transform the candidates."
  (interactive)
  (let* ((spec-arg-elisp (evalator-context-get-special-arg evalator-context-elisp))
         (spec-arg-curr  (evalator-context-get-special-arg (plist-get evalator-state :context)))
         (expr-str (if (equal spec-arg-elisp spec-arg-curr)
                       helm-pattern
                     (replace-regexp-in-string spec-arg-curr spec-arg-elisp helm-pattern))))
    (condition-case err
        (message
         (prin1-to-string
          (evalator-context-elisp-transform-candidates (evalator-get-candidates) expr-str nil)))
      (error
       (message err)))))

(defun evalator-action-confirm-make-or-transform (&optional f-and-args)
  "Make initial candidates or transform candidates then update history.
Accepts an optional arg F-AND-ARGS to pass to `evalator-make-or-transform'."
  (interactive)
  (let* ((err-handler (lambda (err-str)
                        (message (concat "Error: " err-str))
                        nil))
         (cands (evalator-candidate-make-or-transform f-and-args err-handler)))
    (when cands
      (evalator-history-push! cands helm-pattern)
      (helm-unmark-all)
      (helm-set-pattern ""))))

(defun evalator-action-confirm-transform-collect ()
  "Transform the entire candidate selection then update history.
Normally candidates are transformed by evaluating the current
expression on each candidate with the special arg referring to the
value of the candidate.  This action changes that behavior in two
ways.  First, it changes the meaning of the special arg so it refers
to the entire candidate selection.  Second, the current expression is
evaluated only once to produce a single candidate.  This action is
used for when you need to produce an aggregate result."
  (interactive)
  (let* ((f (slot-value (plist-get evalator-state :context) :transform-candidates))
         (expr-str helm-pattern)
         (mode (plist-get evalator-state :mode))
         (args (list (evalator-get-candidates) expr-str mode t)))
    (evalator-action-confirm-make-or-transform (list f args))))

(defun evalator-action-insert-special-arg ()
  "Insert the evalator special arg into the expression prompt."
  (interactive)
  (insert (evalator-context-get-special-arg (plist-get evalator-state :context))))

(defun evalator-flash (status)
  "Change the evalator expression prompt face according to STATUS."
  (let ((f (if (equal :success status) 'evalator-success 'evalator-error)))
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (face-remap-add-relative 'minibuffer-prompt f))))

(cl-defun evalator-marked-candidates (&key with-wildcard)
  "Same as 'helm-marked-candidates' except it returns nil if no candidates were marked."
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
  "Return persistent help string."
  (cl-flet ((f (command)
               (key-description (where-is-internal command evalator-key-map t))))
    (concat "History forward, "
            (f 'evalator-action-previous) ": History backward, "
            (f 'evalator-action-confirm) ": Accept transformation, "
            (f 'evalator-action-insert-special-arg) ": Insert special arg")))

(defun evalator-get-candidates ()
  "Return current evalator candidates.
If there are marked candidates, return the list of those.  Otherwise,
return a list of all the candidates."
  (let ((cands-all (evalator-history-current :candidates))
        (cands-marked (evalator-marked-candidates)))
    (or cands-marked cands-all)))

(defun evalator-try-context-f (context-f args &optional err-handler)
  "Try executing the given evaluation context function CONTEXT-F.
Calls TRANSFORM-F with the given ARGS.  Returns the result if the
operation was successful.  If there was an error and the optional arg
ERR-HANDLER is nil, then return all current evalator candidates.  If
ERR-HANDLER is non-nil, then it is executed and its value is returned."
  (condition-case err
      (progn
        (if (equal "" helm-pattern)
            (signal 'evalator-error '("Empty Expression"))
          (progn (evalator-flash :success)
                 (apply context-f args))))
    (error
     (evalator-flash :error)
     (if err-handler
         (funcall err-handler (prin1-to-string err))
       (evalator-history-current :candidates)))))

(defun evalator-candidate-make-or-transform (&optional f-and-args err-handler)
  "Make initial candidates or transform current candidates.
If current history index is 0 then the context's `:make-candidates'
slot function and appropriate args are passed to
`evalator-try-context-f' for evaluation.  Otherwise the context's
`:transform-candidates' slot function is used.  If optional arg
F-AND-ARGS is non-nil then it will be used instead.  Function also
accept's an optional ERR-HANDLER to pass to `evalator-try-context-f'."
  (with-helm-current-buffer
    (if f-and-args
        (apply 'evalator-try-context-f (append f-and-args (list err-handler)))
      (let* ((make-f      (slot-value (plist-get evalator-state :context) :make-candidates))
             (transform-f (slot-value (plist-get evalator-state :context) :transform-candidates))
             (expr-str    helm-pattern)
             (mode        (plist-get evalator-state :mode))
             (f-and-args  (if (equal 0 (evalator-history-index))
                              (list make-f (list expr-str mode t) err-handler)
                            (list transform-f (list (evalator-get-candidates) expr-str mode) err-handler))))
        (apply 'evalator-try-context-f f-and-args)))))

(defun evalator-build-source (candidates mode)
  "Build the source for a evalator session using a CANDIDATES list and a MODE."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :candidates candidates
    :filtered-candidate-transformer (lambda (_c _s) (evalator-candidate-make-or-transform))
    :keymap evalator-key-map
    :nohighlight t
    :nomark (equal :explicit mode)
    :persistent-help (evalator-persistent-help)
    :volatile t))

(defun evalator-build-history-source ()
  "Build a source that will show the current point in history."
  (helm-build-dummy-source "Evalator History"
    :filtered-candidate-transformer (lambda (_c _s)
                                      (list (funcall evalator-prompt-f)))
    :header-line "History"
    :nomark t
    :nohighlight t))

(defun evalator-insert-equiv-expr ()
  "Insert the equivalent expression of the previous evalator session into the current buffer."
  (interactive)
  (if (equal :explicit (plist-get evalator-state :mode))
      (insert (funcall
               (slot-value (plist-get evalator-state :context) :make-equiv-expr)
               (evalator-history-expression-chain)))
    (message "Error: This command is only allowed when the last evalator session was started with `evalator-explicit'.")))

(defun evalator-resume ()
  "Resume last evalator session."
  (interactive)
  (helm-resume "*helm-evalator*"))

(defun evalator (&optional mode)
  "Start an evalator session.  Accepts an optional MODE."
  (interactive)
  (evalator-state-init mode)
  (evalator-history-push! evalator-candidates-initial "")
  (let* ((helm-after-update-hook (copy-sequence helm-after-update-hook))
         (history-source (evalator-build-history-source))
         (result-source (evalator-build-source evalator-candidates-initial mode)))

    ;; Prevent history candidate from being selected
    (add-hook 'helm-after-update-hook (lambda ()
                                        (helm-next-line)))
    
    (helm :sources (list history-source result-source)
          :buffer "*helm-evalator*"
          :prompt "Enter Expression: "
          :helm-after-update-hook helm-after-update-hook)))

(defun evalator-explicit ()
  "Start an evalator-session in explicit mode.
In explicit mode the data generated will always appear as a single
candidate.  This is the only mode that allows an equivalanet
expression to be generated."
  (interactive)
  (evalator :explicit))

(provide 'evalator)

;;; evalator.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator.el ends here
