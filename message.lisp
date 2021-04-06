;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass port ()
  ((channel :initform nil :initarg :channel))
  (:default-initargs
   :channel (make-instance 'chanl:channel)))

(defclass msg ()
  ((payload :initform nil :initarg :payload)
   (reply :initform nil :initarg :reply)))

(defgeneric sendmsg (p data &key reply)
  (:documentation "Send a message over a port, returning the receipt"))

(defgeneric getreply (m &key blockp)
  (:documentation "Get a message's reply"))

(defgeneric message (p data &key reply)
  (:documentation "Send a message over a port, returning the reply"))

(defgeneric reply (m data &key blockp)
  (:documentation "Reply to a message"))

(defgeneric waitformsg (p)
  (:documentation "Wait to recieve a message"))

(defun return-new-chan (a)
  (if a
	  (make-instance 'chanl:channel)
	  nil))

(defmethod sendmsg ((p port) data &key (reply t))
  (let ((newmsg (make-instance 'msg
							   :payload data
							   :reply (return-new-chan reply))))
	(chanl:send (slot-value p 'channel) newmsg)
	newmsg))

(defmethod getreply ((m msg) &key (blockp t))
  (with-slots (reply) m
	(if (null reply)
		nil
		(let ((rval (multiple-value-list
					 (chanl:recv reply
								 :blockp blockp))))
		  (if (null (cadr rval))
			  (list nil nil)
			  (list (car rval) t))))))

(defmethod message ((p port) data &key (reply t))
  (let ((msg (sendmsg p data :reply reply)))
	(if reply
		(getreply msg)
		nil)))

(defmethod reply ((m msg) data &key (blockp t))
  (with-slots (reply) m
	(if (null reply)
		nil
		(chanl:send reply data :blockp blockp))))

(defmethod waitformsg ((p port))
  (with-slots (channel) p
	(chanl:recv channel)))
