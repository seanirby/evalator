;;; evalator.el --- Emacs package for interactive transformation of data
;; 
;; Copyright © , Sean Irby
;; Author: Sean Irby
;; Maintainer: Sean Irby <sean.t.irby@gmail.com>
;; URL: http://www.github.com/seanirby/evalator
;; Version: 0.0.1
;; Keywords: languages, elisp, helm
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


(defvar evalator-candidates-initial '("Enter an expression below to generate initial data"))
(defvar evalator-error nil)
(put 'evalator-error 'error-conditions '(error))

(defvar evalator-prompt-f
  (lambda ()
    (format "%s of %s" (+ 1 (evalator-history-index)) (length (evalator-history))))
  "Points to a function that is called with the current history index
and length.  Will be used to generate the evalator prompt") 

(defun evalator-action-previous ()
  "Go to the next history state and update the evalator session."
  (interactive)
  (when (not (equal 0 (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ -1 (evalator-history-index)))
    (evalator-unmark-all)
    (helm-set-pattern "")
    (helm-update)))

(defun evalator-action-next ()
  "Go to the previous history state and update the evalator session."
  (interactive)
  (when (not (equal (+ -1 (length (evalator-history))) (evalator-history-index)))
    (evalator-utils-put! evalator-state :history-index (+ 1 (evalator-history-index)))
    (evalator-unmark-all)
    (helm-set-pattern "")
    (helm-update)))

(defun evalator-action-execute-in-elisp ()
  ""
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

(defun evalator-action-confirm (&optional f-and-args)
  "Transform current candidates.
If successful, pushes the result onto the evalator history."
  (interactive)
  (let* ((err-handler (lambda (err-str)
                        (message (concat "Error: " err-str))
                        nil))
         (cands (evalator-candidate-transformer f-and-args err-handler)))
    (when cands
      (evalator-history-push! cands helm-pattern)
      (evalator-unmark-all)
      (helm-set-pattern ""))))

(defun evalator-action-confirm-collect ()
  ""
  (interactive)
  (let* ((f (slot-value (plist-get evalator-state :context) :transform-candidates))
         (expr-str helm-pattern)
         (mode (plist-get evalator-state :mode))
         (args (list (evalator-get-candidates) expr-str mode t)))
    (evalator-action-confirm (list f args))))

(defun evalator-action-insert-special-arg ()
  "Insert the special evalator arg into the expression prompt."
  (interactive)
  (insert (evalator-context-get-special-arg (plist-get evalator-state :context))))

(defun evalator-flash (status)
  "Change the evalator expression prompt face according to STATUS."
  (let ((f (if (equal :success status) 'evalator-success 'evalator-error)))
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (face-remap-add-relative 'minibuffer-prompt f))))

(defun evalator-unmark-all ()
  "Same as 'helm-unmark-all' except no message."
  (interactive)
  (with-helm-window
    (save-excursion
      (helm-clear-visible-mark))
    (setq helm-marked-candidates nil)
    (helm-mark-current-line)
    (helm-display-mode-line (helm-get-current-source))))

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
  ""
  (let ((cands-all (evalator-history-current :candidates))
        (cands-marked (evalator-marked-candidates)))
    (or cands-marked cands-all)))

(defun evalator-try-context-candidate-f (context-f args err-handler)
  "Call the context's `:make-candidates' or `:transform-candidates' function.
Attempt to make or transform the current evalator candidates according
to the input expression EXPR and mode MODE.  Arguments MAKE-F and
TRANSFORM-F are references to the current evaluation context's
`:make-candidates' function and `:transform-candidates' function,
respectively.  If the history index is 0 MAKE-F is called.  Otherwise,
TRANSFORM-F is called.  If optional arg, ERR-HANDLER, is nil and the
operation fails the current evalator candidates are returned.
Otherwise, ERR-HANDLER is called with the error string and that result
is returned."
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

(defun evalator-candidate-transformer (&optional f-and-args err-handler)
  "Can specify your own F-AND-ARGS list."
  (with-helm-current-buffer
    (if f-and-args
        (apply 'evalator-try-context-candidate-f (append f-and-args (list err-handler)))
      (let* ((make-f      (slot-value (plist-get evalator-state :context) :make-candidates))
             (transform-f (slot-value (plist-get evalator-state :context) :transform-candidates))
             (expr-str    helm-pattern)
             (mode        (plist-get evalator-state :mode))
             (f-and-args  (if (equal 0 (evalator-history-index))
                              (list make-f (list expr-str mode t) err-handler)
                            (list transform-f (list (evalator-get-candidates) expr-str mode) err-handler))))
        (apply 'evalator-try-context-candidate-f f-and-args)))))

(defun evalator-build-source (candidates mode)
  "Build the source for a evalator session using a CANDIDATES list and a MODE."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :candidates candidates
    :filtered-candidate-transformer (lambda (_c _s) (evalator-candidate-transformer))
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

(defun evalator-insert-equiv-expr (&optional exprs)
  "Insert the equivalent expression of the previous evalator session.
If EXPRS is nil then the expression list will be retrieved from the
evalator-history will be used instead."
  (interactive)
  (insert (funcall
           (slot-value (plist-get evalator-state :context) :make-equiv-expr)
           (or (evalator-history-expression-chain) exprs))))

(defun evalator-resume ()
  "Resumes last evalator session."
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
          :prompt "Enter Expression: ")))

(defun evalator-explicit ()
  "Start an evalator-session in explicit mode."
  (interactive)
  (evalator :explicit))

(provide 'evalator)

;;; evalator.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator.el ends here
