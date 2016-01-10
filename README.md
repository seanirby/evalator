# EVALATOR #

## Introduction ##

Evalator is an Emacs plugin built on top of Helm that allows for interactive evaluation and transformation of data.  You can use Evalator for several things but here's a few:

* Test a calculation
* Generate code
* Build and execute a complex shell command
* Interact with Emacs from a non-Elisp environment

Lets go through a few examples to see what Evalator can do.

Suppose you wanted to generate several configuration files in your current directory.  The files should be named "setup-windows.el", "setup-keys.el", and "setup-packages.el".  Each file should look like the following:
```
;; setup-windows.el
(provide 'setup-windows)
```

You can do this manually but that would be inefficient.  With Evalator you can do this quickly.  See below:

*INSERT GIF HERE*

Evalator isn't just limited to Elisp.  If you've installed the appropriate context package you can use Evalator in any language you'd like.  In the following example, I'm working in a Clojure file and I'd like to filter the even numbers from a list of integers, convert them to strings, and then concatenate the result.  Unfortunately, I've forgotten the function that converts data to a string.  I exit Evalator to lookup the function, copy it, restart Evalator, and paste the function back into my expression.  Once I'm satisfied with my result, I exit Evalator and insert the equivalent code of the entire session into my Clojure file.

*INSERT GIF HERE*

What if you made a mistake?  Suppose you wanted to filter the odd numbers instead of the even ones.  Evalator lets you move backwards and forwards throughout the session so you can edit any expression in the evaluation history.

*INSERT GIF HERE*

As you can see, Evalator is quite powerful.  It is similar to using a REPL, but instead of executing only a single expression at a time, Evalator lets you incrementally build a result based off of as many commands as you'd like.

## Installation ##
## Setup ##
## Basic Usage ##
### Special Arguments ###
### History ###
### Normal Mode ###
### Explicit Mode ###
## Language Support ##
Evalator ships with an Elisp evaluation context, but it is built to support any language that can be evaluated within Emacs.  See [here](https://github.com/seanirby/evalator-context-cider) for a Clojure/Clojurescript evaluation context using CIDER.

