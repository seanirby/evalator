;;; evalator.el --- Package for interactive transformation of data with helm
;;
;; Copyright © , Sean Irby
;; Author: Sean Irby
;; Maintainer: Sean Irby <sean.t.irby@gmail.com>
;; URL: http://www.github.com/seanirby/evalator
;; Version: 1.0.0
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
(require 'evalator-config)
(require 'evalator-context)
(require 'evalator-elisp)
(require 'evalator-faces)
(require 'evalator-history)
(require 'evalator-key-map)
(require 'evalator-state)
(require 'evalator-utils)
(require 'helm)

(defvar evalator-help-message
  "* Evalator Help

Evalator is a new kind of REPL for Emacs.  It lets you interactively
transform data and can support many different languages.

Since the evalator documentation is in a state of flux currently, this
help will only reference things that aren't likely to change.

NOTE: This help also includes the help for helm, which evalator depends
on as a front-end.  Knowing some basic helm commands is helpful to using
evalator to its full potential.  The generic helm help is located directly
under this help section.

** Evalator Session Shortcuts

The shortcuts in the table below are used while in the evalator buffer.

 |----------+-------------------------------------------+------------------------------------------|
 | Shortcut | Command                                   | Description                              |
 |----------+-------------------------------------------+------------------------------------------|
 |          |                                           |                                          |
 | RETURN   | evalator-action-confirm-make-or-transform | Confirm the expression and either        |
 |          |                                           | make the initial candidates or transform |
 |          |                                           | the existing ones.                       |
 |          |                                           |                                          |
 |          |                                           | If this action is executed at the first  |
 |          |                                           | evalator prompt then confirming will     |
 |          |                                           | generate the initial candidates.         |
 |          |                                           |                                          |
 |          |                                           | Otherwise, confirmation evaluates the    |
 |          |                                           | expression on each candidate.            |
 |          |                                           |                                          |
 |----------+-------------------------------------------+------------------------------------------|
 |          |                                           |                                          |
 | C-c ;    | evalator-action-insert-special-arg        | Insert the special arg character         |
 |          |                                           |                                          |
 | or       |                                           | See the \"Special Args\" section below   |
 |          |                                           | for details on using special args.       |
 | C-;      |                                           |                                          |
 |          |                                           |                                          |
 |----------+-------------------------------------------+------------------------------------------|
 |          |                                           |                                          |
 | C-c C-c  | evalator-action-confirm-transform-collect | This command causes the candidates to    |
 |          |                                           | be treated as a single collection.       |
 |          |                                           |                                          |
 |          |                                           | This means that the special arg will     |
 |          |                                           | refer to the entire collection of        |
 |          |                                           | candidates.  The expression will only be |
 |          |                                           | evaluated on this collection and only    |
 |          |                                           | one candidate will be produced as a      |
 |          |                                           | result.                                  |
 |          |                                           |                                          |
 |          |                                           | This command is useful for generating    |
 |          |                                           | an aggregate result.                     |
 |          |                                           |                                          |
 |----------+-------------------------------------------+------------------------------------------|
 |          |                                           |                                          |
 | C-c C-e  | evalator-action-execute-in-elisp          | Execute the expression on each candidate |
 |          |                                           | using Elisp.                             |
 |          |                                           |                                          |
 |          |                                           | NOTE: This action does not transform the |
 |          |                                           | candidates                               |
 |          |                                           |                                          |
 |----------+-------------------------------------------+------------------------------------------|
 |          |                                           |                                          |
 | C-j      | evalator-action-next                      | Go to next history state                 |
 |          |                                           |                                          |
 |----------+-------------------------------------------+------------------------------------------|
 |          |                                           |                                          |
 | C-l      | evalator-action-previous                  | Go to previous history state             |
 |          |                                           |                                          |
 |----------+-------------------------------------------+------------------------------------------|

** Global commands

Below are the global evalator commands that can be run using M-x.

You should probably bind them to shortcut.

|----------------------------+-----------------------------------------------------------------------------------|
| Command                    | Description                                                                       |
|----------------------------+-----------------------------------------------------------------------------------|
|                            |                                                                                   |
| evalator                   | Starts an evalator session.                                                       |
|                            |                                                                                   |
|                            | Command accepts an optional mode and evaluation context as arguments.             |
|                            |                                                                                   |
|                            | If the mode arg is nil, then normal mode is used.                                 |
|                            |                                                                                   |
|                            | If the evaluation context arg is nil, then the felisp evaluation context is used. |
|                            |                                                                                   |
|----------------------------+-----------------------------------------------------------------------------------|
|                            |                                                                                   |
| evalator-explicit          | Starts an evalator session in explicit mode.                                      |
|                            |                                                                                   |
|                            | Accepts an optional evaluation context                                            |
|                            |                                                                                   |
|----------------------------+-----------------------------------------------------------------------------------|
|                            |                                                                                   |
| evalator-resume            | Resumes your last evalator session                                                |
|                            |                                                                                   |
|----------------------------+-----------------------------------------------------------------------------------|
|                            |                                                                                   |
| evalator-insert-equiv-expr | Inserts the equivalent expression of the last evalator session into the           |
|                            | current buffer.                                                                   |
|                            |                                                                                   |
|                            | NOTE: The last session must have been run in explicit mode for this to work.      |
|                            |                                                                                   |
|----------------------------+-----------------------------------------------------------------------------------|

** Special Args

Special arguments are used to refer to the value of the candidate being
transformed.  The default special character is Ⓔ which is the unicode
character x24ba.  If a candidate was equal to the number 1 and your
expression was (+ 1 Ⓔ) then 1 would be substituted for the special arg
and the result would evaluate to 2.

You can insert the special arg into the expression prompt by using the
shortcut C-c ; which executes `evalator-action-insert-special-arg'.
An abbreviated shortcut C-; has been provided for GUI Emacs users has

If the candidate is a collection you can refer to an element within it
by adding an integer after the special arg.  So if a candidate is the
vector [1 2 3 4] and your expression is (+ 1 Ⓔ0) then Ⓔ0 would be
substituted with 1 and the result would evaluate to 2.

")

(defvar evalator-expression-prompt "Enter Expression: ")
(defvar evalator-candidates-initial '("Enter an expression below to generate initial data")
  "Informational candidate used on evalator startup.")

(defvar evalator-error nil
  "Error symbol used for signaling evalator errors.")
(put 'evalator-error 'error-conditions '(error))

(defun evalator-action-previous ()
  "Go to the previous state and update the evalator session."
  (interactive)
  (when (not (equal 0 (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ -1 (evalator-history-index)))
    (helm-unmark-all)
    (helm-set-pattern "")
    (helm-update)))

(defun evalator-action-next ()
  "Go to the next history state and update the evalator session."
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
  (let* ((spec-arg-elisp (evalator-context-get-special-arg (evalator-elisp-context)))
         (spec-arg-curr  (evalator-context-get-special-arg (evalator-state-context)))
         (expr-str (if (equal spec-arg-elisp spec-arg-curr)
                       helm-pattern
                     (replace-regexp-in-string spec-arg-curr spec-arg-elisp helm-pattern))))
    (condition-case err
        (message
         (prin1-to-string
          (evalator-elisp-transform-candidates (evalator-get-candidates) expr-str nil)))
      (error
       (evalator-message err)))))

(defun evalator-action-confirm-make-or-transform (&optional f-and-args)
  "Make initial candidates or transform candidates then update history.
Accepts an optional arg F-AND-ARGS to pass to `evalator-make-or-transform'."
  (interactive)
  (let* ((err-handler (lambda (err-str)
                        (evalator-message (concat "Error: " err-str))
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
  (let* ((f (slot-value (evalator-state-context) :transform-candidates))
         (expr-str helm-pattern)
         (args (list (evalator-get-candidates) expr-str t)))
    (evalator-action-confirm-make-or-transform (list f args))))

(defun evalator-action-insert-special-arg ()
  "Insert the evalator special arg into the expression prompt."
  (interactive)
  (insert (evalator-context-get-special-arg (evalator-state-context))))

(defun evalator-message (msg)
  "Output MSG and append a newline and an instruction to continue."
  (read-char (concat msg "\n" "Press any key to return to minibuffer."))
  ;; Hack needed because minibuffer window doesn't resize after printing a multiline message
  (with-current-buffer (window-buffer (active-minibuffer-window))
    (let ((txt (minibuffer-contents)))
      (delete-minibuffer-contents)
      (insert txt))))

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
            (f 'evalator-action-confirm-make-or-transform) ": Accept transformation, "
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
      (let* ((make-f      (slot-value (evalator-state-context) :make-candidates))
             (transform-f (slot-value (evalator-state-context) :transform-candidates))
             (expr-str    helm-pattern)
             (mode        (plist-get evalator-state :mode))
             (f-and-args  (if (equal 0 (evalator-history-index))
                              (list make-f (list expr-str mode) err-handler)
                            (list transform-f (list (evalator-get-candidates) expr-str nil) err-handler))))
        (apply 'evalator-try-context-f f-and-args)))))

(defun evalator-build-source (candidates mode)
  "Build the source for a evalator session using a CANDIDATES list and a MODE."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :candidates candidates
    :filtered-candidate-transformer (lambda (_c _s) (evalator-candidate-make-or-transform))
    :help-message evalator-help-message
    :keymap evalator-key-map
    :multiline t
    :nohighlight t
    :nomark (equal :explicit mode)
    :persistent-help (evalator-persistent-help)
    :volatile t))

(defun evalator-build-history-source ()
  "Build a source that will show the current point in history."
  (helm-build-dummy-source "Evalator History"
    :filtered-candidate-transformer (lambda (_c _s)
                                      (list (funcall evalator-config-prompt-f)))
    :header-line "History"
    :nomark t
    :nohighlight t))

;;;###autoload
(defun evalator-insert-equiv-expr ()
  "Insert the equivalent expression of the previous evalator session into the current buffer."
  (interactive)
  (if (equal :explicit (plist-get evalator-state :mode))
      (insert (funcall
               (slot-value (evalator-state-context) :make-equiv-expr)
               (evalator-history-expression-chain)))
    (message "Error: This command is only allowed when the last evalator session in explicit mode.")))

;;;###autoload
(defun evalator-resume ()
  "Resume last evalator session."
  (interactive)
  (let ((print-circle t)) ;; Necessary to support circular lists
    (helm-resume "*helm-evalator*")))

;;;###autoload
(defun evalator (&optional mode context)
  "Start an evalator session.

Function accepts an optional MODE keyword and a CONTEXT symbol.

If MODE is non-nil and a currently supported mode value then that mode
will be used for the session.

Below are currently supported values for MODE:

`:explicit'

If MODE is nil evalator will start in normal mode.

If CONTEXT is non-nil, then the result of calling CONTEXT's function
definition will be used as the session's evaluation context.

If CONTEXT is nil, then the current buffer's major mode will be
searched for in `evalator-config-mode-context-alist'.  If a match is
found, the context associated with that major mode is used in the
evalator session.  If no match is found, an elisp evaluation context
is used instead.
"
  (interactive)
  (when (evalator-state-init mode (evalator-context-get context))
    (add-hook 'minibuffer-setup-hook (lambda ()
                                       (setq-local minibuffer-message-timeout nil) t nil))
    (evalator-history-push! evalator-candidates-initial "")
    (let* ((print-circle t) ;; Necessary to support circular lists
           (evalator-after-update-hook (copy-sequence helm-after-update-hook))
           (history-source (evalator-build-history-source))
           (result-source (evalator-build-source evalator-candidates-initial mode)))

      ;; Prevent history candidate from being selected
      (add-hook 'evalator-after-update-hook (lambda ()
                                              (helm-next-line)))

      (helm :sources (list history-source result-source)
            :buffer "*helm-evalator*"
            :prompt "Enter Expression: "
            :helm-after-update-hook evalator-after-update-hook))))

;;;###autoload
(defun evalator-explicit (&optional context)
  "Helper function to start an evalator-session in explicit mode.

In explicit mode the data generated will always be represented as a
single candidate.  This is the only mode that allows an equivalent
expression of the session to be generated through
`evalator-insert-equiv-expr'.
"
  (interactive)
  (evalator :explicit context))

(provide 'evalator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator.el ends here
