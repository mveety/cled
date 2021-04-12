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
   (wincurline :initform 0)
   (wincurcol :initform 0)
   (topline :initform 0 :initarg :topline)
   (lines :initform 0 :initarg :lines)
   (cols :initform 0 :initarg :cols)
   (buffer :initform nil :initarg :buffer)
   (buf-data :initform nil)
   (wind-data :initform nil))
  (:default-initargs
   :manager-id (string (gensym "WINDOW"))))

(defvar *window-cmd-template* nil
  "Command template for windows")

(defun getrval (reply)
  (if (null (getf reply :status))
	  (getf reply :returns)
	  :error))

(defun dot-equal (dot1 dot2)
  (and (eq (car dot1) (car dot2))
	   (eq (cadr dot1) (cadr dot2))))

(defgeneric update-window-data (win))
(defgeneric format-window-data (win))
(defgeneric scroll-down (win))
(defgeneric scroll-up (win))
(defgeneric page-down (win))
(defgeneric page-up (win))
(defgeneric update-window-cursor-location (win))
(defgeneric win-cursor-up (win))
(defgeneric win-cursor-down (win))
(defgeneric get-win-update (win))
(defgeneric window-resize (win nlines ncols))

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
	  (setf wind-data (reverse new-wind-data)))))

(defmethod scroll-down ((win window))
  (with-slots (topline buffer lines) win
	(let ((nlines (getrval (sendcmd buffer :nlines)))
		  (newtopline (1+ topline)))
	  (if (not (>= newtopline (1- nlines)))
		  (setf topline newtopline)
		  (setf topline (1- nlines))))))

(defmethod scroll-up ((win window))
  (with-slots (topline buffer lines) win
	(let ((newtopline (1- topline)))
	  (if (not (<= newtopline 1))
		  (setf topline newtopline)
		  (setf topline 1)))))

(defmethod page-down ((win window))
  (with-slots (lines) win
	(dotimes (x lines)
	  (scroll-down win))))

(defmethod page-up ((win window))
  (with-slots (lines) win
	(dotimes (x lines)
	  (scroll-up win))))

(defmethod update-window-cursor-location ((win window))
  (with-slots (buffer lines cols curline curcol
			   wincurline wincurcol) win
	  (when (> curcol cols)
		(setf wincurline (1+ curline)
			  wincurcol 0))
	  (when (> wincurline lines)
		(scroll-down win) ;; i'm assuming the cursor won't move
		(update-window-cursor-location win))))

(defmethod win-cursor-up ((win window))
  (with-slots (curline curcol topline lines buffer) win
	(sendcmd buffer :cursor-up)
	(let ((curdot (getrval (sendcmd buffer :get-cursor))))
	  (setf curline (car curdot)
			curcol (cadr curdot))
	  (when (< curline topline)
		(scroll-up win)))))

(defmethod win-cursor-down ((win window))
  (with-slots (curline curcol topline lines buffer) win
	(sendcmd buffer :cursor-down)
	(let ((curdot (getrval (sendcmd buffer :get-cursor))))
	  (setf curline (car curdot)
			curcol (cadr curdot))
	  (when (> curline (+ topline lines))
		(scroll-down win)))))

(defmethod get-win-update ((win window))
  (update-window-data win)
  (format-window-data win)
  (update-window-cursor-location win)
  (with-slots (wind-data wincurline wincurcol) win
	(list '(:dot :lines)
		  (list wincurline wincurcol)
		  wind-data)))

(defmethod window-resize ((win window) nlines ncols)
  (with-slots (lines cols) win
	(setf lines nlines
		  cols ncols)))

;;;;;; command handling for windows ;;;;;;

(defmacro defcmd-win (name fun &optional (nargs nil))
  `(defcommand *window-cmd-template*
	 ,name
	 ,fun
	 :nargs ,nargs
	 :object t))

(defcmd-win :cursor-up #'win-cursor-up)
(defcmd-win :cursor-down #'win-cursor-down)
(defcmd-win :window-update #'get-win-update)
(defcmd-win :window-resize #'window-resize 2)

(defun window-process (win)
  (with-slots (buffer) win
	(labels ((default-function (msgdata)
			   (message buffer msgdata)))
	  (run-table win win
				 :default-function #'default-function
				 :end-command :end-command)
	  ;; when the table is done, unset the buffer owner
	  (sendcmd buffer :unset-owner)
	  (sendcmd buffer :hidden)
	  nil)))

(defun make-window (name buffer lines cols &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let ((newwin (make-instance 'window :name name
									   :buffer buffer
									   :lines lines
									   :cols cols)))
	(add-template-to-cmd-table newwin *window-cmd-template* newwin)
	(setf (slot-value newwin 'thread)
		  (bt:make-thread (lambda ()
							(window-process newwin))
						  :name (concatenate 'string "window-thread: " name)))
	(noreturn-sendcmd buffer :set-owner newwin) ;; be sure to tell the buffer who's problem it is
	newwin))
