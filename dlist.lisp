;;; cl-cons-dlist -- Doubly linked lists using conses
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl-cons-dlist
  (:nicknames :dlist)
  (:use :cl)
  (:export #:*element-printing-limit*
		   #:dlist
		   #:dl-length
		   #:dl-append
		   #:dl-push
		   #:dl-insert
		   #:dl-remove
		   #:dl-next
		   #:dl-nextn
		   #:dl-nextn-fast
		   #:dl-prev
		   #:dl-prevn
		   #:dl-prevn-fast
		   #:dl-head
		   #:dl-tail
		   #:dl-data
		   #:dl-append-list
		   #:dl-to-list
		   #:make-dlist))

(in-package :dlist)

(defvar *element-printing-limit* 10)

(defclass dlist ()
  ((length :initform 0)
   (head :initform nil)
   (cur :initform nil)
   (tail :initform nil))
  (:documentation "Container for a doubly linked list. Do be careful
poking around in here. This class contains rather weird circular
lists that lisp chokes on."))

(defun initialize-dlist (list elem)
  (with-slots (length head cur tail) list
	(if (and (null head) (null cur) (null tail))
		(progn
		  (setf head (cons (cons nil nil) elem)
				length 1
				cur head
				tail head)
		  elem)
		nil)))

(defun copy-dlist (list)
  (let ((nlist (make-instance 'dlist)))
	(with-slots (length head cur tail) list
	  (setf (slot-value nlist 'length) length
			(slot-value nlist 'head) head
			(slot-value nlist 'cur) head
			(slot-value nlist 'tail) tail))
	nlist))

(defgeneric dl-length (list)
  (:documentation "Return the length of LIST"))

(defgeneric dl-append (list data)
  (:documentation "Append DATA to the end of the LIST"))

(defgeneric dl-push (list data)
  (:documentation "Push DATA to the head of the list"))

(defgeneric dl-insert (list data)
  (:documentation "Insert DATA just after the cursor"))

(defgeneric dl-remove (list)
  (:documentation "Remove the element at the cursor from LIST"))

(defgeneric dl-next (list)
  (:documentation "Move the cursor one element to the right"))

(defgeneric dl-nextn (list n)
  (:documentation "Move the cursor N elements to the right"))

(defgeneric dl-prev (list)
  (:documentation "Move the cursor one element to the left"))

(defgeneric dl-prevn (list n)
  (:documentation "Move the cursor N elements to the left."))

(defgeneric dl-head (list)
  (:documentation "Move the cursor to the head of the list."))

(defgeneric dl-tail (list)
  (:documentation "Move the cursor to the tail of the list."))

(defgeneric dl-data (list)
  (:documentation "Return the data stored in the element at the cursor."))

(defgeneric (setf dl-data) (new list)
  (:documentation "Store NEW in the element at the cursor."))

(defgeneric dl-append-list (dl list)
  (:documentation "Append LIST to a dlist DL"))

(defgeneric dl-to-list (list)
  (:documentation "Convert a dlist LIST to a normal list"))

(defmethod dl-length ((list dlist))
  (slot-value list 'length))

(defmacro new-element (prev next data)
  `(cons (cons ,prev ,next) ,data))

(defmacro set-next (cur-elem next-elem)
  `(setf (cdr (car ,cur-elem)) ,next-elem))

(defmacro set-prev (cur-elem prev-elem)
  `(setf (car (car ,cur-elem)) ,prev-elem))

(defmacro getnext (elem)
  `(cdr (car ,elem)))

(defmacro getprev (elem)
  `(car (car ,elem)))

(defmethod dl-append ((list dlist) data)
  (with-slots (length head cur tail) list
	(if (and (null head) (null cur) (null tail))
		(initialize-dlist list data)
		(let ((new-elem (new-element nil nil data)))
		  (set-next tail new-elem)
		  (set-prev new-elem tail)
		  (setf tail new-elem)
		  (incf length)
		  data))))

(defmethod dl-push ((list dlist) data)
  (with-slots (length head cur tail) list
	(if (and (null head) (null cur) (null tail))
		(initialize-dlist list data)
		(let ((new-elem (new-element nil nil data)))
		  (set-next new-elem head)
		  (set-prev head new-elem)
		  (setf head new-elem)
		  (incf length)
		  data))))

(defmethod dl-insert ((list dlist) data)
  (with-slots (length head cur tail) list
	(if (eq cur tail)
		(progn
		  (dl-append list data)
		  (set cur tail))
		(progn (let ((new-elem (new-element nil nil data))
					 (cur->next (getnext cur)))
				 (set-prev new-elem cur)
				 (set-next new-elem cur->next)
				 (set-next cur new-elem)
				 (set-prev cur->next new-elem)
				 (setf cur new-elem)
				 (incf length))))
	data))

(defmethod dl-remove ((list dlist))
  (with-slots (length head cur tail) list
	(if (zerop length)
		nil
		(let ((prev (getprev cur))
			  (next (getnext cur)))
		  (cond
			((and (null prev) (null next))
			 (format t "single element list~%")
			 (setf head nil
				   cur nil
				   tail nil))
			((and (null prev) next)
			 (format t "cur = head~%")
			 (set-prev next nil)
			 (setf cur next
				   head next))
			((and prev (null next))
			 (format t "cur = tail~%")
			 (set-next prev nil)
			 (setf cur prev
				   tail prev))
			(t
			 (format t "cur is in the middle~%")
			 (set-prev next prev)
			 (set-next prev next)
			 (setf cur prev)))
		  (decf length)
		  t))))

(defmethod dl-next ((list dlist))
  (with-slots (cur) list
	(if (null (getnext cur))
		nil
		(progn
		  (setf cur (getnext cur))
		  t))))

(defmethod dl-nextn ((list dlist) n)
  (block bail
	(dotimes (x n)
	  (when (null (dl-next list))
		(return-from bail nil)))
	t))

(defmethod dl-nextn-fast ((list dlist) n)
  (with-slots (cur) list
	(let* ((tmp cur)
		   (sc nil)
		   (st (block bail
				 (dotimes (x n)
				   (when (null (setf sc (getnext tmp)))
					 (return-from bail nil))
				   (setf tmp sc))
				 t)))
	  (setf cur tmp)
	  st)))

(defmethod dl-prev ((list dlist))
  (with-slots (cur) list
	(if (null (getprev cur))
		nil
		(progn
		  (setf cur (getprev cur))
		  t))))

(defmethod dl-prevn ((list dlist) n)
  (block bail
	(dotimes (x n)
	  (when (null (dl-prev list))
		(return-from bail nil)))
	t))

(defmethod dl-prevn-fast ((list dlist) n)
  (with-slots (cur) list
	(let* ((tmp cur)
		   (sc nil)
		   (st (block bail
				 (dotimes (x n)
				   (when (null (setf sc (getprev tmp)))
					 (return-from bail nil))
				   (setf tmp sc))
				 t)))
	  (setf cur tmp)
	  st)))

(defmethod dl-head ((list dlist))
  (with-slots (head cur) list
	(setf cur head)
	t))

(defmethod dl-tail ((list dlist))
  (with-slots (cur tail) list
	(setf cur tail)
	t))

(defmethod dl-data ((list dlist))
  (cdr (slot-value list 'cur)))

(defmethod (setf dl-data) (new (list dlist))
  (setf (cdr (slot-value list 'cur)) new))

(defmethod print-object ((list dlist) stream)
  (print-unreadable-object (list stream :type t)
	(let ((tmplst (copy-dlist list))
		  (counter 0))
	  (if (> (dl-length tmplst) 0)
		  (progn
			(format stream "length: ~A, data: " (dl-length tmplst))
			(loop do
			  (format stream "~S" (dl-data tmplst))
			  (if (null (dl-next tmplst))
				  (loop-finish)
				  (if (< counter (1- *element-printing-limit*))
					  (format stream " ")
					  (progn
						(format stream "...")
						(loop-finish))))
				  (incf counter)))
		  (format stream "length: 0, data: nil")))))

(defmethod dl-append-list ((dl dlist) list)
  (dolist (x list)
	(dl-append dl x)))

(defmethod dl-to-list ((list dlist))
  (if (< (dl-length list) 1)
	  nil
	  (let ((tmp nil)
			(tdlist (copy-dlist list)))
		(loop do
		  (push (dl-data tdlist) tmp)
		  (if (null (dl-next tdlist))
			  (loop-finish)))
		(nreverse tmp))))

(defun make-dlist (&optional source-list)
  "Create a new dlist, optionally filled using the contents of SOURCE-LIST"
  (let ((new-dlist (make-instance 'dlist)))
	(unless (null source-list)
	  (dl-append-list new-dlist source-list))
	new-dlist))
