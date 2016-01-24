# EVALATOR #

## Introduction ##

Evalator is an Emacs package built on top of [helm](https://github.com/emacs-helm/helm) that allows for interactive evaluation and transformation of data.  You can use evalator for several things but here's a few:

*   Test a calculation
*   Generate code
*   Build and execute a complex shell command

Evalator uses Elisp by default, but supports other languages through evaluation context packages.  One nice thing about this is you can generate some data using your favorite language and use that as the input to an Emacs command.

Available context packages:

*   [Clojure(via CIDER)](https://github.com/seanirby/evalator-context-cider)(coming soon)

Don't see a context package for your favorite language?  Write one!  See [evalator-context-elisp.el](https://github.com/seanirby/evalator/blob/master/evalator-context-elisp.el) and [evalator-context-cider.el](https://github.com/seanirby/evalator-context-cider/blob/master/evalator-context-cider.el) for examples.  Open an issue in this repo with any questions and I'll be happy to help.

Here's a practical example to demonstrate what evalator's capable of:

### A Practical Example ###
Suppose you have several Emacs packages and want to generate an elisp configuration file for each one in your current directory.  Each file should be named ```setup-$PACKAGE.el``` and each file should contain the line ```(provide 'setup-$PACKAGE)```. ```$PACKAGE``` refers to the package name.  Watch the example below to see how easy this is with evalator.

![a-practical-example](example-gifs/practical.gif)

Here's what happened:
* Copy all package names
* Call ```M-x evalator``` to start an evalator session
* Paste the package names into a quoted lisp list ```'()```
* Build a string for the shell command I want executed for each package
* Call the Emacs command ```shell-command``` with each of the strings from before
* Exit evalator and verify files are created with the right contents

See the [Basic Usage](#basic-usage) section for a detailed walkthrough.

## Installation ##

*UPDATE WHEN ADDED TO MELPA*

## Setup ##

Add the following to your init file
```
(require 'evalator)
;; Suggested keybindings
(global-set-key (kbd "C-c e v") 'evalator)
(global-set-key (kbd "C-c e e") 'evalator-explicit)
(global-set-key (kbd "C-c e r") 'evalator-resume)
(global-set-key (kbd "C-c e i") 'evalator-insert-equiv-expr)
```

## <a name="basic-usage"></a> Basic Usage ##

![walkthrough](example-gifs/walkthrough.gif)

### Public API ###

Command                                          | Description
-------------------------------------------------|---------------------------
```evalator```                                   | Starts an evalator session 
```evalator-explicit```                          | Starts an evalator session in explicit mode
```evalator-resume```                            | Resumes last evalator session
```evalator-insert-equiv-expr```                 | Inserts the equivalent expression of the last evalator session into the current buffer. NOTE: The last session must have been run in explicit mode for this to work.

### Key Actions ###
Below is a table of evalator specific key actions that can be used within the evalator session.  For helm specific commands such as candidate navigation and marking, refer to [helm's](https://github.com/emacs-helm/helm) documentation or run ```C-h m``` from within the evalator session.

Action                                                | Action shortcut      | Description
------------------------------------------------------|----------------------|-------------------------------
```evalator-action-confirm-make-or-transform```       | <kbd>RET</kbd>       | Accepts initial candidates or transformation then updates history.  If transforming candidates, the expression is evaluated on each candidate with the special arg referring to the candidate's value
```evalator-action-confirm-transform-collection```    | <kbd>C-c C-c</kbd>   | Accepts transformation and updates history. Expression is evaluated once with the special arg referring to the selected candidates list
```evalator-action-execute-in-elisp```                | <kbd>C-c C-e</kbd>   | Executes the expression in Elisp on each selected candidate.  The history is not updated and the candidates are not transformed
```evalator-action-insert-special-arg```              | <kbd>C-;</kbd>       | Inserts the current evaluation context's special arg
```evalator-action-next```                            | <kbd>C-j</kbd>       | Goes to next history state
```evalator-action-previous```                        | <kbd>C-l</kbd>       | Goes to previous history state
### Special Arguments ###
TODO
### History ###
TODO
### Normal Mode ###
TODO
### Explicit Mode ###
TODO
