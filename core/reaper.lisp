;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass grim-reaper (process port)
  ((type :initform 'grim-reaper)
   (type-string :initform "reaper")
   (name :initform "the grim reaper")
   (restartable :initform nil))
  (:default-initargs
   :manager-id (string (gensym "REAPER"))))

(defvar *grim-reaper* nil)
(defvar *reaper-messages* t)
(defvar *reaper-stream* t)
(defvar *reaper-print-name* t)

(defmacro reaper-format (string &rest args)
  `(if *reaper-messages* (format *reaper-stream* ,string ,@args)))

(defun name-or-object (proc)
  (if *reaper-print-name*
      (format nil "~A (~A)" (slot-value proc 'name) (slot-value proc 'manager-id))
      proc))

;;; do note: the grim reaper does not reply to messages because it assumes
;;; the sender is deceased.
(defmethod proc-entry ((reaper grim-reaper))
  (let ((msg nil)
	(msgdata nil))
    (loop do
      (setf msg (waitformsg reaper))
      (setf msgdata (slot-value msg 'payload))
      (setf (slot-value reaper 'inflight-message) nil)
      (when (equal (car msgdata) :restart)
	(if (slot-value (cadr msgdata) 'restartable)
	    (if (start-process (cadr msgdata))
		(reaper-format "grim-reaper: restarted process ~A~%" (name-or-object (cadr msgdata)))
		(reaper-format "grim-reaper: unable to restart process ~A~%" (name-or-object (cadr msgdata))))
	    (reaper-format "grim-reaper: disallowed from restarting process ~A~%" (name-or-object (cadr msgdata))))
	(force-output t))
      (when (equal (car msgdata) :stop)
	(loop-finish)))))

(defmethod proc-exit ((reaper grim-reaper))
  (reaper-format "grim-reaper: exiting~%")
  (force-output t))

(defun start-grim-reaper ()
  (if (null *grim-reaper*)
      (let ((new-reaper (make-instance 'grim-reaper)))
	(setf *grim-reaper* new-reaper)
	(start-process new-reaper)
	t)
      nil))

(defun alert-reaper (proc)
  (when (and (not (null *grim-reaper*))
	     (slot-value proc 'restartable))
    (let ((newmsg (make-instance 'msg
				 :payload (list :restart proc)
				 :reply nil)))
      (chanl:send (slot-value *grim-reaper* 'channel) newmsg :blockp nil)
      nil)))

(defun stop-reaper ()
  (delete-process *grim-reaper*)
  (setf *grim-reaper* nil))
