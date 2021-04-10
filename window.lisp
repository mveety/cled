;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass window (port command-table)
  ((name :initform "unamed window" :initarg :name)
   (manager-id :initform nil :initarg :manager-id)
   (thread :initform nil)
   (curline :initform 0 :initarg :curline)
   (curcol :initform 0 :initarg :curcol)
   (topline :initform 0 :initarg :topline)
   (lines :initform 0 :initarg :lines)
   (cols :initform 0 :initarg :cols)
   (buffer :initform nil :initarg :buffer)
   (buf-data :initform nil)
   (wind-data :initform nil))
  (:default-initargs
   :manager-id (string (gensym "WINDOW"))))

(defun getrval (reply)
  (if (null (getf reply :status))
	  (getf reply :returns)
	  :error))

(defun dot-equal (dot1 dot2)
  (and (eq (car dot1) (car dot2))
	   (eq (cadr dot1) (cadr dot2))))

(defgeneric update-window-data (win))
(defgeneric format-window-data (win))


(defmethod update-window-data ((win window))
  (with-slots (topline curcol curline
			   lines buffer buf-data) win
	(let ((buf-update (getrval (sendcmd buffer
										:get-update
										topline
										lines
										:full (if (null buf-data)
												  t
												  nil)))))
	  (if (equal buf-update :error)
		  nil
		  (progn
			(when (member :dot (car buf-update))
			  (setf curcol (car (cadr buf-update))
					curline (cadr (cadr buf-update))))
			(when (member :lines (car buf-update))
			  (setf buf-data (caddr buf-update)))
			t)))))

(defmethod format-window-data ((win window))
  (with-slots (lines cols buf-data wind-data) win
	(let ((new-wind-data nil)
		  (cur-line nil)
		  (curx 0)
		  (cury 0))
	  (dolist (line buf-data)
		(when (< cury lines)
		  (dolist (c line)
			(push c cur-line)
			(incf curx)
			(when (>= curx cols)
			  (push (reverse cur-line) new-wind-data)
			  (setf cur-line nil
					curx 0)))
		  (when (not (null cur-line))
			(push (reverse cur-line) new-wind-data))
		  (setf cur-line nil
				curx 0)
		  (incf cury)))
	  (setf wind-data new-wind-data))))
