;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled)

(defclass input-process (port process)
  ((type :initform 'process)
   (type-string :initform "input")
   (viewer :initform nil)
   (last-ncols :initform 0)
   (last-nlines :initform 0)
   ))

(defmethod proc-exit ((p input-process))
  (when (message-in-flight-p p)
    (reply (get-in-flight-message p)
	   (list :status :process-error :returns nil)
	   :blockp nil))
  (alert-reaper p))

(defmethod proc-entry ((p input-process))
  (with-slots (viewer last-ncols last-nlines) p
    (let ((k nil)
	  (event nil))
      (loop named input-loop
	    do (if (checkformsg p)
		   (let ((msg (waitformsg p)))
		     (with-slots ((payload cled-core::payload)) msg
		       ;; totally break semantics here. reply before acting.
		       ;; this isn't totally needed, but will prevent a
		       ;; process-error reply.
		       (reply msg (list :status nil :return t) :blockp nil)
		       (case (car payload)
			 (:end-command
			  (no-process-restart p)
			  (return-from input-loop))
			 (:restart
			  (return-from input-loop))
			 )))
		   ;; gather events
		   (progn
		     (when (null event) ;; resize event
		       (multiple-value-bind (ncols nlines)
			   (charms:window-dimensions charms:*standard-window*)
			 (when (or (not (= ncols last-ncols))
				   (not (= nlines last-nlines)))
			   (setf last-ncols ncols
				 last-nlines nlines)
			   (setf event (list :resize nlines ncols)))))
		     (when (null event) ;; keypress
		       (setf k (get-canonical-key))
		       (unless (null k)
			 (setf event (list :keypress k))))
		     ;; send events if required
		     (when (not (null event))
		       ;; Don't ask or wait for a reply.
		       (apply #'noreturn-sendcmd (append (list viewer) event))
		       (setf event nil))
		     ;; rate control sleep. I only want to sleep when servicing
		     ;; the keyboard. If I get a message this will get me back to
		     ;; doing my job as quickly as possible.
		     (sleep (/ 1 *keyboard-polling-rate*))))))))
