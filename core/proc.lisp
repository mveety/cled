;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass process ()
  ((type :initform 'process)
   (type-string :initform "process")
   (manager-id :initform nil :initarg :manager-id)
   (name :initform "unamed" :initarg :name)
   (restartable :initform nil)
   (thread :initform nil :initarg :thread))
  (:default-initargs
   :manager-id (string (gensym "PROCESS"))))

(defvar *process-lock* (bt:make-lock "process lock"))
(defvar *all-processes* (make-hash-table :test 'equal))
(defvar *running-processes* (make-hash-table :test 'equal))

(defun add-hash (ht key value)
  (bt:with-lock-held (*process-lock*)
	(setf (gethash key ht) value)))

(defun rem-hash (ht key)
  (bt:with-lock-held (*process-lock*)
	(remhash key ht)))

(defgeneric proc-entry (proc))
(defgeneric proc-exit (proc))
(defgeneric start-process (proc))
(defgeneric start-process-in-current-thread (proc))
(defgeneric stop-process (proc))
(defgeneric delete-process (proc))
(defgeneric no-process-restart (proc))
(defgeneric process-restart (proc))

(defmethod initialize-instance :after ((proc process) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (add-hash *all-processes* (slot-value proc 'manager-id) proc))

(defmethod proc-entry ((proc process))
  ;; this does nothing but waste cycles. define a new one
  (loop
    (sleep 1)))

(defmethod proc-exit ((proc process))
  ;; by default nothing happens
  nil)

(defmethod start-process ((proc process))
  (with-slots (thread manager-id type-string name restartable) proc
    (if (and (not (null (gethash manager-id *all-processes*)))
	     (null (gethash manager-id *running-processes*)))
	(progn
	  (setf restartable t
		thread (bt:make-thread
			(lambda () (unwind-protect
					(progn
					  (add-hash *running-processes* manager-id proc)
					  (proc-entry proc))
				     (rem-hash *running-processes* manager-id)
				     (proc-exit proc)))
			:name (concatenate 'string type-string "-thread: " name)))
	  t)
	nil)))

(defmethod start-process-in-current-thread ((proc process))
  (with-slots (thread manager-id type-string name restartable) proc
    (if (and (not (null (gethash manager-id *all-processes*)))
	     (null (gethash manager-id *running-processes*)))
	(progn
	  (setf restartable nil ;; this might be restartable. idk how sbcl would handle this
		thread (bt:current-thread))
	  (unwind-protect
	       (progn
		 (add-hash *running-processes* manager-id proc)
		 (proc-entry proc))
	    (rem-hash *running-processes* manager-id)
	    (proc-exit proc))
	  t)
	nil)))

(defmethod stop-process ((proc process))
  (with-slots (thread manager-id restartable) proc
    (rem-hash *running-processes* manager-id)
    (setf restartable nil)
    (when (bt:thread-alive-p thread)
      (bt:destroy-thread thread))))

(defmethod delete-process ((proc process))
  (with-slots (manager-id) proc
    (stop-process proc)
    (rem-hash *all-processes* manager-id)))

(defmethod no-process-restart ((proc process))
  (setf (slot-value proc 'restartable) nil))

(defmethod process-restart ((proc process))
  (setf (slot-value proc 'restartable) t))

(defun all-processes ()
  (hash-table-alist *all-processes*))

(defun running-processes ()
  (hash-table-alist *running-processes*))
