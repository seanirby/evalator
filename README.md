# helm-lambda #

## Introduction ##

helm-lambda is an Emacs plugin that allows for interactive evaluation and transformation of data.  You can use helm-lambda to do several different things:

* Test a calculation
* Generate code
* Build and execute a complex shell command
* Browse documentation.

This is better explained with a few examples.

Suppose you wanted to generate several configuration files in your current directory.  The files should be named "setup-windows.el", "setup-keys.el", and "setup-packages.el".  Each file should look like the following:
```
;; setup-windows.el
(provide 'setup-windows)
```

You can do this manually but that would be inefficient.  With helm-lambda you can do this quickly.  See below:

*INSERT GIF HERE*

Helm-lambda isn't just limited to Elisp.  If you've installed the appropriate context package you can use helm-lambda in any language you'd like.  In the following example, I'm working in a Clojure file and I'd like to transform a list of integers to a list of integer strings.  Unfortunately, I've forgotten the function that converts data to a string.  I use helm-lambda to browse the available functions in the current namespace.  When I'm satisfied with the result, I exit helm-lambda and insert the equivalent code of the entire session.

*INSERT GIF HERE*

As you can see, helm-lambda is quite powerful.  It is similar to using a REPL, but instead of being able to execute only a single command and seeing the result, helm-lambda lets you incrementally build a result based off of as many commands as you'd like.

## Installation ##
## Setup ##
## Basic Usage ##
### Special Arguments ###
### History ###
### Normal Mode ###
### Explicit Mode ###
## Language Support ##
helm-lambda ships with an Elisp evaluation context, but it is built to support any language that can be evaluated within Emacs.  See [here](https://github.com/seanirby/helm-lambda-context-cider) for a Clojure/Clojurescript evaluation context using CIDER.

