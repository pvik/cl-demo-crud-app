(defpackage db
  (:use :cl :postmodern :local-time)
  (:export :connect))
(in-package :db)

(defvar *todo-table* 'sample.todo)

(defstruct todo
  id item note completed created-date completed-date)

(defun connect (host username password database)  
  (if (and postmodern:*database*
		   (postmodern:connected-p postmodern:*database*))
	  (format t "Already connected to a DB!~%")
	  (progn
		(setf postmodern:*database*
			  (postmodern:connect database username password host :pooled-p t))
		(format t "~&Connected to ~a@~a/~a!" username host database))))

(defun list-to-todo (todo-i)
  (let ((id             (first todo-i))
		(item           (second todo-i))
		(note           (third todo-i))
		(completed      (fourth todo-i))
		(created-date   (fifth todo-i))
		(completed-date (sixth todo-i)))
	(make-todo
	 :id             id
	 :item           item
	 :note           note
	 :completed      completed
	 :created-date   (unless (equal created-date :null)
					   (local-time:universal-to-timestamp created-date))
	 :completed-date (unless (equal completed-date :null)
					   (local-time:universal-to-timestamp completed-date)))))

(defmacro get-todo (&rest where-clause)
  `(let ((todo-i (mapcar #'list-to-todo
						 (postmodern:query
						  (:select 'id 'item 'note 'completed 'create_date 'completed_date
								   :from *todo-table*
								   :where ,@where-clause)))))
	 todo-i))

(defun get-todo-by-id (id)
  (first (get-todo (:= 'id id))))

(defmethod insert ((todo-i todo))
  (let* ((id             (todo-id todo-i))
		 (item           (todo-item todo-i))
		 (note           (todo-note todo-i))
		 (completed      (todo-completed todo-i))
		 (created-date   (if (todo-created-date todo-i)
							 (local-time:format-timestring nil
														   (todo-created-date todo-i))
							 (local-time:format-timestring nil
														   (local-time:now))))
		 (completed-date (if (todo-completed-date todo-i)
							 (local-time:format-timestring nil
														   (todo-completed-date todo-i))
							 (if completed (local-time:format-timestring nil
																		 (local-time:now))
								 :NULL))))
	(if id
		;; id exists -> update record
		(progn
		  (format t "updating record ~d~&" id)
		  (postmodern:query
		   (:update *todo-table*
					:set
					'item item 'note note 'completed completed
					'create_date created-date
					'completed_date completed-date
					:where (:= 'id id))))
		;; insert record
		(progn
		  (format t "inserting ~a~&" item)
		  (postmodern:query
		   (:insert-into *todo-table*
						 :set
						 'item item 'note note 'completed completed
						 'create_date created-date
						 'completed_date completed-date))))))
