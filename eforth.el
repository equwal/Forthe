;;; eforth.el --- FORTH in elisp. -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-22
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
;; Keywords:
;; URL: https://spensertruex.com/eforth
;; Homepage:
;; This file is not part of GNU Emacs.
;; This file is free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.
;;; Commentary:
;; Version 0.0.1
;;
;; Docs available as: HTML | PDF | Plain Text
;;
;; This is an implementation of an elisp-based FORTH. This means that elisp
;; is used to write the function definitions. In fact, no FORTH function
;; definitions are provided at all to the eForth macro from the library.
;; The main benefit comes from reducing the verbosity of elisp in a small
;; Domain Specific Language the programmer defines.
;;
;; (require 'eforth)
;; (defun 8pan ()
;;   (interactive)
;;   (eforth '((n . (lambda () (switch-to-buffer nil)))
;;             (d . split-window-below)
;;             (r . split-window-right)
;;             (wr . windmove-right)
;;             (wl . windmove-left)
;;             (wd . windmove-down)
;;             (wu . windmove-up)
;;             (del . delete-other-windows)
;;             (cf . make-frame-command)
;;             (nf . other-frame))
;;           (cf del r r wr wr r d wr d wl wl d wl d wr n
;;               wr n wr n wl wl wl wd n wr n wr n wr n wl
;;               wl wl wu nf)))
;;
;;
;;
;; HOW DOES A FORTH WORK?
;;
;;
;; Forth code is gone through by the pointer moving through the call stack.
;; When a non-function is found, it is added to the argument stack. When a
;; function is found, it uses up the argument stack if it has arguments, or
;; just runs if it is nullary. Nothing else is provided by the
;; implementation.
;;
;;
;;
;; WHY?
;;
;;
;; Needing to simulate a long set of keypresses to create an eight panel
;; frame (37 in total), the best option seemeed to be obviously to create a
;; miniature language with 1 or 2 letter abbreviations for each. Any
;; arguments to a function can be passed before it in the list, and there
;; is no need to define a return stack.
;;
;; M-x 8pan will produce a new frame with eight panels (emacs-speak:
;;                                                      "windows"). Here is how it works:
;;
;; eforth is called with two arguments: the environment, name to function
;; bindings, and the eforth code list. You can see in this example that
;; there are no arguments used, since all of these are nullary functions.
;; Here is how you might use arguments:
;;
;; (eforth '((ff . find-file)) ("~/some-file" ff))
;;
;; Here we ran (find-file "~/some-file"). If you need to reuse a forth,
;; just define an *env* variable:
;;
;; (defvar *env* '((>r . to-return-stack) ...))
;; (eforth *env* (forth code here ...))
;;
;;
;;
;; BENEFITS
;;
;;
;; Concise code.
;;
;; Elisp's convention of names like:
;;
;; org--insert-structure-template-unique-keys
;;
;; (a real function in the org mode) result in extremely verbose code.
;; Using short, lexical names can reduce code size by a large portion.
;;
;;
;; Compiles to Elisp.
;;
;; Elisp is hated by many, and for good reason. It is an outdated and
;; clunky lisp. Many have tried and failed to port Emacs to Common Lisp,
;; without success. Forth, unlike modern lisps, is designed to be easy to
;; implement. Writing a forth compiler is in fact a trivial exercise.
;;
;;
;;
;; DRAWBACKS
;;
;;
;; Elispers might not want to read your eFORTH code
;;
;;
;; eForth is not stable
;;
;; Forth is space-age technology, and eForth is young. Send a pull request.
;;
;;
;; Isn't verbose code self-documenting?
;;
;; NO. Documented code is documented. Using short names with the bindings
;; handily available in the same form is equally explicit as writing them
;; out every time.
;;
;;
;;
;; CONTRIBUTING
;;
;;
;; Please make sure your contributions are melpa-friendly, and documented
;; in the README. You can use pandoc-mode to transform the README.org into
;; plaintext `;;; Commentary:`.

;;; Code:
(require 'cl-lib)

(defun eforth--inner (lang list)
  "Internal function for the `eforth' macro.
LANG are the bindings for the forth.
LIST is full of the forth stack (RPN)."
  (let ((s))
    (cl-flet ((fn (fn) (if (functionp fn)
                           fn
                         (symbol-function fn))))
      (dolist (i list)
        (if (assoc i lang)
            (let ((fn (cadr (assoc i lang))))
              (if s
                  (progn (apply (fn fn) s) (setf s nil))
                (funcall (fn fn))))
          (push i s))))))

(defmacro eforth (lang list)
  "Define and execute a forth language.
LANG are the let-bindings for the FORTH's operators.
LIST is the forth's stack."
  `(eforth--inner ,lang ',list))

(provide 'eforth)

                                        ;Internal function for the `eforth' macro.ends here
