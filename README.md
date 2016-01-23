# EVALATOR #

## Introduction ##

Evalator is an Emacs extension built on top of Helm that allows for interactive evaluation and transformation of data.  You can use Evalator for several things but here's a few:

* Test a calculation
* Generate code
* Build and execute a complex shell command
* Interact with Emacs from a non-Elisp environment

I think it's best to start with an example to show what Evalator can do.

### A Practical Example ###
Suppose you have several Emacs packages and want to generate an elisp configuration file for each one in your current directory.  Each file should be named ```setup-$PACKAGE.el``` and each file should contain the line ```(provide 'setup-$PACKAGE)```. ```$PACKAGE``` refers to the package name.  Watch the example below to see how I can do this easily with Evalator.

![a-practical-example](example-gifs/practical.gif)

Here's what happened:
* Copy all package names
* Call ```M-x evalator``` to start an Evalator session
* Paste the package names into a quoted lisp list ```'()```
* Build a string for the shell command I want executed for each package
* Call the Emacs command ```shell-command``` with each of the strings from before
* Exit Evalator and verify files are created with the right contents

See the following examples for a more detailed explanation of how to use Evalator. 

### Special Args ###
![special-args-example](example-gifs/special-args.gif)

Upon starting any evalator session you must first provide an expression to generate the initial data.  In this example, the expression ```(list 0 1 2 3)``` is used to generate the integers 0 through 3.  Notice that the prompt is red until a valid expression is entered.  I then press ```RETURN``` which is bound to the command ```evalator-action-confirm```.  This will evaluate my expression, store the result, and prompt me to enter a new expression.  Now I enter an expression to add 1 to every integer.  You'll notice that I entered a special character ```Ⓔ```.  In normal operation, evalator evaluates the expression on each item and ```Ⓔ``` refers to the value of that item.  The special arg is customizable for each evaluation context(more on those later) and it can be quickly entered using the the shortcut, ```C-;```, which is bound to the command ```evalator-action-insert-special-arg```.  I confirm this transformation and enter a new expression ```(cons Ⓔ (list Ⓔ))```.  This adds the number to a list containing itself.  After confirming this transformation,  I select the first and third items, then enter the expression ```(car 'Ⓔ)``` which takes the first element from the list.  Note the quotation before the special arg.  The special arg refers to the item value, and since ```(car (1 1))``` is an invalid expression, the quote must be added to prevent ```(1 1)``` from being evaluated.  After I confirm the transformation, you'll see that only the items I've marked appear in the next step.  Finally,  I select all candidates using ```M-a```(helm-mark-all) and then insert them into the buffer above using ```C-c TAB```(helm-copy-to-buffer).
    
* ADD MORE EXAMPLES *

As you can see, Evalator is quite powerful.  It is similar to using a REPL, but instead of executing only a single expression at a time, Evalator lets you incrementally build a result based off of as many commands as you'd like.

## Installation ##

*UPDATE WHEN ADDED TO MELPA*

## Setup ##

```
(require 'evalator)
;; Suggested keybinding
(global-set-key (kbd "C-c e v") 'evalator)
```

## Basic Usage ##
### Public API ###

Command                                          | Description
-------------------------------------------------|---------------------------
```evalator```                                   | Starts an evalator session 
```evalator-explicit```                          | Starts an evalator session in explicit mode
```evalator-resume```                            | Resumes last evalator session
```evalator-insert-equiv-expr```                 | Inserts the equivalent expression of the last evalator session into the current buffer. NOTE: The last session must have been run in explicit mode for this to work.

### Key Actions ###
Below is a table of evalator specific key actions that can be used in an evalator session.  For helm specific commands such as candidate navigation and marking, refer to [helm's](https://github.com/emacs-helm/helm) documentation or run ```C-h m``` from within an evalator session.

Action shortcut      | Description
---------------------|-------------------------------
<kbd>RET</kbd>       | Accepts initial candidates or transformation then updates history.  If transforming candidates, the expression is evaluated on each candidate with the special arg referring to the candidate's value
<kbd>C-c C-c</kbd>   | Accepts transformation and updates history. Expression is evaluated once with the special arg referring to the selected candidates list
<kbd>C-c C-e</kbd>   | Executes the expression in Elisp on each selected candidate.  The history is not updated and the candidates are not transformed
<kbd>C-;</kbd>       | Inserts the current evaluation context's special arg
<kbd>C-j</kbd>       | Goes to next history state
<kbd>C-l</kbd>       | Goes to previous history state
### Special Arguments ###
### History ###
### Normal Mode ###
### Explicit Mode ###
## Language Support ##
Evalator ships with an Elisp evaluation context, but it is built to support any language that can be evaluated within Emacs.  See [here](https://github.com/seanirby/evalator-context-cider) for a Clojure/Clojurescript evaluation context using CIDER.

