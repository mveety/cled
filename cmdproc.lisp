;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass command-table ()
  ((commands :initarg :commands)
   (cmdlist :initform nil :initarg :cmdlist))
  (:default-initargs
   :commands (make-hash-table :size 15)))

(defgeneric add-command (tbl name fun &key nargs object))
(defgeneric command-exists (tbl name))
(defgeneric find-command (tbl name))
(defgeneric del-command (tbl name))
(defgeneric run-command (tbl name &rest args))
(defgeneric run-table (p tbl &key end-command default-function))
(defgeneric sendcmd (p name &rest args))
(defgeneric get-command-list (tbl))

(defmethod add-command ((tbl command-table) name fun &key (nargs nil) (object nil))
  (with-slots (commands cmdlist) tbl
	(setf (gethash name commands) (list :name name :fun fun :nargs nargs :prepend object))
	(when (not (member name cmdlist))
	  (push name cmdlist))))

(defmethod command-exists ((tbl command-table) name)
  (cadr (multiple-value-list (gethash name (slot-value tbl 'commands)))))

(defmethod find-command ((tbl command-table) name)
  (let ((cmd (multiple-value-list (gethash name (slot-value tbl 'commands)))))
	(if (cadr cmd)
		(car cmd)
		nil)))

(defmethod del-command ((tb command-table) name)
  (remhash name (slot-value tb 'commands))
  (setf (slot-value tb 'cmdlist) (remove name (slot-value tb 'cmdlist))))

(defun run-command-1 (cmd args)
  (list :status nil :returns
		(apply (getf cmd :fun) (if (null (getf cmd :prepend))
								   args
								   (cons (getf cmd :prepend) args)))))

(defmethod run-command ((tbl command-table) name &rest args)
  (let* ((cmd (find-command tbl name))
		 (nargs (getf cmd :nargs)))
	(if (null cmd)
		(list :status :not-found :returns nil)
		(if (numberp nargs)
			(if (= nargs (length args))
				(run-command-1 cmd args)
				(list :status :args-error :returns nil))
			(list :status nil :returns
				  (if (equal nargs t)
					  (run-command-1 cmd args)
					  (if (not (getf cmd :prepend))
						  (funcall (getf cmd :fun))
						  (funcall (getf cmd :fun) (getf cmd :prepend)))))))))
						  

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
				  ;; the default function is if you need to send an unhandled command down
				  ;; the chain. This means we just pass the straight return values to the
				  ;; caller.
				  (reply tmpmsg (funcall default-function msgdata))
				  (reply tmpmsg (list :status :unknown-command :returns nil))))))))

(defmethod sendcmd ((p port) name &rest args)
  (let ((rval (message p (append (list name) args))))
	(if (null (cadr rval))
		(list :status :failed-reply :returns nil)
		(car rval))))

(defmethod get-command-list ((tbl command-table))
  (slot-value tbl 'cmdlist))
