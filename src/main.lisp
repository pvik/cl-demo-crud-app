(defpackage cl-demo-crud-app
  (:use :cl :db :local-time)
  (:export :main))
(in-package :cl-demo-crud-app)

(defparameter *allowed-commands* '(show new completed edit))

(defun show (&optional type id)
  (cond
	((eq type 'completed) (print-todo-list
						   (db:get-todo-by-completed t)))
	((eq type 'id) (if (integerp (eval id))
					   (print-todo-detail
						(db:get-todo-by-id (eval id)))
					   (format t "Invalid ID")))
	(t (print-todo-list
		(db:get-todo-by-completed nil)))))

(defun new ()
  (let ((item (progn
				(format t "Todo: ")
				(read-line)))
		(note (progn
				(format t "Note: ")
				(read-line))))
	(db:insert (db:make-todo :item item :note note))))

(defun completed (id)
  (let ((todo-i (db:get-todo-by-id (eval id))))
	(setf (todo-completed todo-i) t)
	(db:insert todo-i)))

(defun edit (id)
  (let* ((todo-i (db:get-todo-by-id (eval id)))
		 (item (progn
				 (format t "Todo: ")
				 (read-line)))
		 (note (progn
				 (format t "Note: ")
				 (read-line))))
	(setf (todo-item todo-i) item)
	(setf (todo-note todo-i) note)
	(db:insert todo-i)))

(defun app-read ()
  (format t "~&> ")
  (let* ((*package* (find-package :cl-demo-crud-app))
		 (cmd (read-from-string
			   (concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
			 (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun app-eval (sexp)
  (if (find (car sexp) *allowed-commands* :test #'eq)
	  (eval sexp)
	  (format t "Unknown Command~&")))

(defun print-todo (todo-i)
  (if (db:todo-p todo-i)
	  (format t "ID:~d [~a] ~30a <~a> <~a>~&"
			  (db:todo-id todo-i)
			  (if (db:todo-completed todo-i) "x" " ")
			  (db:todo-item todo-i)
			  (if (todo-created-date todo-i)
				  (local-time:format-timestring
				   nil
				   (todo-created-date todo-i))
				  " ")
			  (if (and (db:todo-completed todo-i)
					   (todo-completed-date todo-i))
				  (local-time:format-timestring
				   nil
				   (todo-completed-date todo-i))
				  " "))
	  (format t "Item not found!")))

(defun print-todo-detail (todo-i)
  (if (db:todo-p todo-i)
	  (progn
		(print-todo todo-i)
		(format t "~&Note: ~a~&"
				(db:todo-note todo-i)))
	  (format t "Item not found!")))

(defun print-todo-list (todo-list)
  (dolist (todo-i todo-list)
	(print-todo todo-i)))

(defun repl ()
  (let ((cmd (app-read)))
	(if (eq (car cmd) 'quit)
		(format t "Bye!~&")
		(progn
		  (app-eval cmd)
		  (repl)))))

(defun main ()
  (db:connect-db "localhost" "postgres" "docker" "sample_crud")
  (princ "Welcome!")
  (fresh-line)
  (repl))
