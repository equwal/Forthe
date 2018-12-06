eFORTH
===
This is an implementation of an elisp-based FORTH. This means that elisp is used to write the function definitions. In fact, no FORTH function definitions are provided at all to the eforth macro from the library. **The main benefit comes from reducing the verbosity of elisp in a small Domain Specific Language the programmer defines.**
# How?
Forth code is gone through by the pointer moving through the call stack. When a non-function is found, it is added to the argument stack. When a function is found, it uses up the argument stack if it has arguments, or just runs if it is nullary. Nothing else is provided by the implementation.
# Why?
Needing to simulate a long set of keypresses to create an eight panel frame (37 in total), 
the best option seemeed to be obviously to create a miniature language with 1 or 2 letter
abbreviations for each. Any arguments to a function can be passed before it in the list,
and there is no need to define a return stack.
# Usage:
example:

```elisp
(require 'eforth)
(defun 8pan ()
  (interactive)
  (eforth '((n . (lambda () (switch-to-buffer nil)))
	   (d . split-window-below)
	   (r . split-window-right)
	   (wr . windmove-right)
	   (wl . windmove-left)
	   (wd . windmove-down)
	   (wu . windmove-up)
	   (del . delete-other-windows)
	   (cf . make-frame-command)
	   (nf . other-frame))
	  (cf del r r wr wr r d wr d wl wl d wl d wr n wr n wr n wl wl wl wd n
	       wr n wr n wr n wl wl wl wu nf)))

```

`M-x 8pan` will produce a new frame with eight panels (emacs-speak: "windows"). Here is how it works:

`eforth` is called with two arguments: the environment, name to function bindings, and the eforth code list. You can see in this example
that there are no arguments used, since all of these are nullary functions. Here is how you might use arguments:


```elisp
(eforth '((ff . find-file)) ("~/some-file" ff))
```

Here we ran `(find-file "~/some-file")`. If you need to reuse a forth, just define an `*env*` variable:

``` elisp
(defvar *env* '((>r . to-return-stack) ...))
(eforth *env* (forth code here ...))

```
