;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass buffer (process port command-table)
  ((type :initform 'buffer)
   (type-string :initform "buffer"))
  (:default-initargs
   :manager-id (string (gensym "BUFFER"))))

(defvar *buffer-types* nil)

(defgeneric buffer-type (buf)
  (:method ((buf buffer))
	(slot-value buf 'type)))

(defgeneric buffer-type-string (buf)
  (:method ((buf buffer))
	(slot-value buf 'type-string)))

(defgeneric buffer-name (buf)
  (:method ((buf buffer))
	(slot-value buf 'name)))

(defgeneric (setf buffer-name) (val buf)
  (:method (val (buf buffer))
	(setf (slot-value buf 'name) val)))

(defgeneric buffer-id (buf)
  (:method ((buf buffer))
	(slot-value buf 'manager-id)))

(defun make-buffer (type name &rest args)
  (let ((res (assoc type *buffer-types*)))
	(if (null res)
		nil
		(apply (cdr res) (cons name args)))))

(defmacro define-buffer-type (type init-function)
  `(eval-when (:load-toplevel :execute)
	 (push (cons ,type ,init-function) *buffer-types*)))
