;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass buffer (port command-table)
  ((type :initform 'buffer)
   (type-string :initform "buffer")
   (name :initform "unnamed" :initarg :name)
   (thread :initform nil :initarg :thread)))

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

(defun make-buffer (type name &rest args)
  (let ((res (assoc type *buffer-types*)))
	(if (null res)
		nil
		(apply (cdr res) (cons name args)))))

(defmacro define-buffer-type (type init-function)
  `(eval-when (:load-toplevel :execute)
	 (push (cons ,type ,init-function) *buffer-types*)))
