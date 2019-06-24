;;; forthe.el --- FORTH in elisp. -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-22
;; Version: 0.0.2
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience abbrev tools extensions
;; URL: https://spensertruex.com/forthe
;; Homepage:
;; This file is not part of GNU Emacs.
;; This file is free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.
;;; Commentary:
;; Version 0.0.1
;;
;; Docs available as: HTML | ORG-mode
;;
;; Code on github
;;
;; This is an implementation of an elisp-based FORTH. This means that elisp
;; is used to write the function definitions. In fact, no FORTH function
;; definitions are provided at all to the Forthe macro from the library.
;; The main benefit comes from reducing the verbosity of elisp in a small
;; Domain Specific Language the programmer defines.

;;; Code:

(defun forthe--inner (lang list)
  "Internal function for the `forthe' macro.
LANG are the bindings for the forth.
LIST is full of the Forthe stack (RPN)."
  (let (s)
    (dolist (i list)
      (if (assoc i lang)
          (let ((fn (cadr (assoc i lang))))
            (if s
                (progn (apply fn s) (setf s nil))
              (funcall fn)))
        (push i s)))))

(defmacro forthe (lang list)
  "Define and execute a FORTH language (called Forthe).
LANG are the let-bindings for the FORTH's operators.
LIST is the forth's stack."
  `(forthe--inner ,lang ',list))

(provide 'forthe)
;; forthe.el ends here
