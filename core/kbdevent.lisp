;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass kbd-event-table ()
  ((hash-table :initarg :hash-table)
   (default-event :initarg :default))
  (:default-initargs
   :hash-table (make-hash-table :test 'equal)
   :default (list :event :insert :use-key nil :use-char t)))

(defgeneric define-kbd-event (event-table canonical-key event &key use-key use-char))
(defgeneric define-default-kbd-event (event-table canonical-key event &key use-key use-char))
(defgeneric get-kbd-event (event-table canonical-key))
(defgeneric generate-kbd-event (event-table canonical-key))
(defgeneric send-kbd-event (event-table canonical-key port))

(defmethod define-kbd-event ((event-table kbd-event-table) canonical-key event
			     &key (use-key nil) (use-char nil))
  (with-slots (hash-table) event-table
    (setf (gethash canonical-key hash-table)
	  (list :event event :use-key use-key :use-char use-char))))

(defmethod define-default-kbd-event ((event-table kbd-event-table) canonical-key event
				     &key (use-key nil) (use-char nil))
  (with-slots (default-event) event-table
    (setf default-event (list :event event :use-key use-key :use-char use-char))))

(defmethod get-kbd-event ((event-table kbd-event-table) canonical-key)
  (with-slots (hash-table default-event) event-table
    (let ((event (gethash canonical-key hash-table)))
      (if (null event)
	  default-event
	  event))))

(defmethod generate-kbd-event ((event-table kbd-event-table) canonical-key)
  (let ((event (get-kbd-event event-table canonical-key)))
    (cond
      ((getf event :use-key) (list (getf event :event) canonical-key))
      ((getf event :use-char) (let* ((slist (coerce canonical-key 'list))
				     (char (car (last slist)))) ;; the actual char is always last
				(list (getf event :event) char)))
      (t (getf event :event)))))

(defmethod send-kbd-event ((event-table kbd-event-table) canonical-key (p port))
  (apply #'sendcmd (append (list p) (generate-kbd-event event-table canonical-key))))

(defun make-kbd-event-table () (make-instance 'kbd-event-table))
