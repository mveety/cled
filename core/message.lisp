;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass port ()
  ((channel :initform nil :initarg :channel)
   (inflight-message :initform nil))
  (:default-initargs
   :channel (make-instance 'chanl:channel)))

(defclass msg ()
  ((payload :initform nil :initarg :payload)
   (reply :initform nil :initarg :reply)
   (inflight-port :initform nil)))

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

(defgeneric message-in-flight-p (p)
  (:documentation "Check to see if there is a message that has been recv'd but not replied to"))

(defgeneric get-in-flight-message (p)
  (:documentation "Get the in flight message"))

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
  (with-slots (reply inflight-port) m
	(prog1
		(if (null reply)
			nil
			(chanl:send reply data :blockp blockp))
	  (setf (slot-value inflight-port 'inflight-message) nil))))

(defmethod waitformsg ((p port))
  (with-slots (channel inflight-message) p
	(let ((msg (chanl:recv channel)))
	  (setf inflight-message msg)
	  (setf (slot-value msg 'inflight-port) p)
	  msg)))

(defmethod message-in-flight-p ((p port))
  (with-slots (inflight-message) p
	(if (not (null inflight-message))
		t
		nil)))

(defmethod get-in-flight-message ((p port))
  (with-slots (inflight-message) p
	(prog1
		inflight-message
	  (setf inflight-message nil))))
