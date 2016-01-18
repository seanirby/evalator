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
(require 'evalator-faces)
(require 'evalator-history)
(require 'evalator-key-map)
(require 'evalator-state)
(require 'helm)


(defvar evalator-candidates-initial '("Enter an expression below to generate initial data"))
(defvar evalator-error nil)
(put 'evalator-error 'error-conditions '(error))

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

(defun evalator-action-confirm ()
  "Transform current candidates.
If successful, pushes the result onto the evalator history."
  (interactive)
  (let* ((err-handler (lambda (err-str)
                        (evalator-flash :error)
                        (message (concat "Error: " err-str))
                        nil))
         (candidates (evalator-make-or-transform-candidates
                      helm-pattern
                      (plist-get evalator-state :mode)
                      (slot-value (plist-get evalator-state :context) :make-candidates)
                      (slot-value (plist-get evalator-state :context) :transform-candidates)
                      err-handler)))
    (when candidates
      (evalator-history-push! candidates helm-pattern)
      (evalator-unmark-all)
      (helm-set-pattern ""))))

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

(defun evalator-build-source (candidates mode)
  "Build the source for a evalator session using a CANDIDATES list and a MODE."
  (helm-build-sync-source (concat "Evaluation Result" (when (equal :explicit mode) "(Explicit)"))
    :candidates candidates
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      (with-helm-current-buffer
                                        (evalator-make-or-transform-candidates
                                         helm-pattern
                                         (plist-get evalator-state :mode)
                                         (slot-value (plist-get evalator-state :context) :make-candidates)
                                         (slot-value (plist-get evalator-state :context) :transform-candidates))))
    :keymap evalator-key-map
    :nohighlight t
    :nomark (equal :explicit mode)
    :persistent-help (evalator-persistent-help)
    :volatile t))

(defun evalator-make-or-transform-candidates (expr mode make-f transform-f &optional err-handler)
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
  (let ((cands-all (evalator-history-current :candidates))
        (cands-marked (evalator-marked-candidates)))
    (condition-case err
        (progn
          (if (equal "" expr)
              (signal 'evalator-error '("Empty Expression"))
            (progn (evalator-flash :success)
                   (if (equal 0 (evalator-history-index))
                       (funcall make-f expr mode t)
                     (funcall transform-f cands-all cands-marked expr mode)))))
      (error
       (if err-handler
           (funcall err-handler (prin1-to-string err))
         (progn
           (evalator-flash :error)
           cands-all))))))

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

  (let* ((source (evalator-build-source evalator-candidates-initial mode)))
    (helm :sources source
          :buffer "*helm-evalator*"
          :prompt "Enter Expression:")))

(defun evalator-explicit ()
  "Start an evalator-session in explicit mode."
  (interactive)
  (evalator :explicit))

(provide 'evalator)

;;; evalator.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalator.el ends here
