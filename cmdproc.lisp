;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass command-table ()
  ((commands :initarg :commands))
  (:default-initargs
   :commands (make-hash-table :size 15)))

(defgeneric add-command (tbl name fun &key nargs object))
(defgeneric command-exists (tbl name))
(defgeneric find-command (tbl name))
(defgeneric del-command (tbl name))
(defgeneric run-command (tbl name &rest args))
(defgeneric run-table (p tbl &key end-command default-function))
(defgeneric sendcmd (p name &rest args))

(defmethod add-command ((tbl command-table) name fun &key (nargs nil) (object nil))
  (with-slots (commands) tbl
	(setf (gethash name commands) (list :name name :fun fun :nargs nargs :prepend object))))

(defmethod command-exists ((tbl command-table) name)
  (cadr (multiple-value-list (gethash name (slot-value tbl 'commands)))))

(defmethod find-command ((tbl command-table) name)
  (let ((cmd (multiple-value-list (gethash name (slot-value tbl 'commands)))))
	(if (cadr cmd)
		(car cmd)
		nil)))

(defmethod del-command ((tb command-table) name)
  (remhash name (slot-value tb 'commands)))

(defmethod run-command ((tbl command-table) name &rest args)
  (let ((cmd (find-command tbl name)))
	(if (null cmd)
		(list :status :not-found :returns nil)
		(if (or (null (getf cmd :nargs)) (zerop (getf cmd :nargs)))
			(list :status nil :returns
				  (if (null (getf cmd :prepend))
					  (funcall (getf cmd :fun))
					  (funcall (getf cmd :fun) (getf cmd :prepend))))
			(if (and (not (= (length args) (getf cmd :nargs)))
					 (not (equal (getf cmd :nargs) t)))
				(list :status :args-error :returns nil)
				(list :status nil :returns
					  (if (null (getf cmd :prepend))
						  (apply (getf cmd :fun) args)
						  (apply (getf cmd :fun)
								 (cons (getf cmd :prepend) args)))))))))

(defmethod run-table ((p port) (tbl command-table) &key (end-command nil) (default-function nil))
  (let ((tmpmsg nil)
		(msgdata nil)
		(cmd-output nil))
	(loop do
	  (setf tmpmsg (waitformsg p))
	  (setf msgdata (slot-value tmpmsg 'payload))
	  (if (command-exists tbl (car msgdata))
		  (progn
			(setf cmd-output (apply #'run-command
									(cons tbl (cons (car msgdata) (cdr msgdata)))))
			(reply tmpmsg cmd-output))
		  (if (eq (car msgdata) end-command)
			  (progn
				(reply tmpmsg (list :status nil :returns t))
				(loop-finish))
			  (if default-function
				  (reply tmpmsg (list :status nil :returns (funcall default-function (cadr msgdata))))
				  (reply tmpmsg (list :status :unknown-command :returns nil))))))))

(defmethod sendcmd ((p port) name &rest args)
  (let ((rval (message p (list name args))))
	(if (null (cadr rval))
		(list :status :failed-reply :returns nil)
		(car rval))))
