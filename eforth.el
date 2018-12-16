(defun add-to-each (it list-of-lists)
  (let ((l))
    (dolist (i list-of-lists (nreverse l))
      (push (list it i) l))))
(defun inner-eforth (lang list)
  "Forth without a return stack."
  (let ((s))
    (cl-flet ((fn (fn) (if (functionp fn)
                           fn
                         (symbol-function fn))))
      (dolist (i list)
        (if (assoc i lang)
            (let ((fn (cdr (assoc i lang))))
              (if s
                  (progn (apply (fn fn) s) (setf s nil))
                (funcall (fn fn))))
          (push i s))))))
(defmacro eforth (lang list)
  `(inner-eforth ,lang ',list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar *my-env* '((ff . find-file)))        ;;
;; (eforth *my-env* ("~/bogus/" ff))            ;;
;; (eforth '((d . split-window-below))          ;;
;;      (d))                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          ;; (cf nf del r r wr wr r d wr d wl wl d wl d wr n wr n wr n wl wl wl wd n
          ;;     wr n wr n wr n wl wl wl wu nf)
          (cf del r r wr wr r d wr d wl wl d wl d
               n wr n wr n wr n wd n wl n wl n wl n wu)
          ))
(provide 'eforth)
