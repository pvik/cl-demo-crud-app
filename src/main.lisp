(defpackage cl-demo-crud-app
  (:use :cl :db)
  (:export :main))
(in-package :cl-demo-crud-app)

(defun get-list )

(defun app-read ()
  (let ((cmd (read-from-string
			  (concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
			 (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun app-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
	  (eval sexp)
	  '(i do not know that command.)))

(defun app-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
												  (prin1-to-string lst))
									 'list)
							 t
							 nil)
				 'string))
  (fresh-line))

(defun repl ()
  (let ((cmd (app-read)))
	(unless (eq (car cmd) 'quit)
	  (app-print (app-eval cmd))
	  (app-repl))))

(defun main ()
  (db:connect)
  (print "Welcome!")
  (repl))
