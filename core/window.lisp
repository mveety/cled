;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass window (process port command-table)
  ((type :initform 'window)
   (type-string :initform "window")
   (curline :initform 0 :initarg :curline) ;; buffer native dot
   (curcol :initform 0 :initarg :curcol) ;; part of the above
   (hasmark :initform nil)
   (markcol :initform 0)
   (markline :initform 0)
   (wincurline :initform 0) ;; terminal native cursor location
   (wincurcol :initform 0)  ;; part of the above
   (topline :initform 0 :initarg :topline) ;; top line shown in the buffer
   (lines :initform 0 :initarg :lines) ;; window height
   (cols :initform 0 :initarg :cols) ;; window width
   (buffer :initform nil :initarg :buffer) ;; backing buffer
   (buf-data :initform nil) ;; fetched buffer data
   (wind-data :initform nil) ;; processed window data
   (line-offset-data :initform nil)
   (shown-lines :initform 0))
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
(defgeneric change-buffer (win buf))

(defmethod update-window-data ((win window))
  (with-slots (topline curcol curline lines
	       buffer buf-data markcol markline
	       hasmark) win
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
	    (let ((dot-data (assoc :dot buf-update))
		  (line-data (assoc :lines buf-update))
		  (mark-data (assoc :mark buf-update)))
	      (setf curline (car (cadr dot-data))
		    curcol (cadr (cadr dot-data)))
	      (when (not (null line-data))
		(setf buf-data (cadr line-data)))
	      (if (not (null mark-data))
		(setf markline (car (cadr mark-data))
		      markcol (cadr (cadr mark-data))
		      hasmark t)
		(setf hasmark nil
		      markline 0
		      markcol 0)))
	    t)))))

(defmethod format-window-data ((win window))
  (with-slots (lines cols buf-data wind-data shown-lines line-offset-data) win
    (let ((new-wind-data nil)
	  (new-line-offset-data)
	  (cur-line nil)
	  (real-lines 0)
	  (curx 0)
	  (cury 0))
      (dolist (line buf-data)
	(when (< cury lines)
	  (push cury new-line-offset-data)
	  (incf real-lines)
	  (dolist (c line)
	    (push c cur-line)
	    (incf curx)
	    (when (>= curx cols)
	      (push (reverse cur-line) new-wind-data)
	      (incf cury)
	      (setf cur-line nil
		    curx 0))
	    )
	  (push (reverse cur-line) new-wind-data)
	  (setf cur-line nil
		curx 0)
	  (incf cury)
	  ))
      (setf shown-lines real-lines
	    wind-data (reverse new-wind-data)
	    line-offset-data (reverse new-line-offset-data)))))

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
      (if (not (<= newtopline 0))
	  (setf topline newtopline)
	  (setf topline 0)))))

(defmethod page-down ((win window))
  (with-slots (lines topline buffer) win
    (dotimes (x (- lines 3))
      (scroll-down win))
    (set-dot buffer (1- (+ topline lines)) 0)))

(defmethod page-up ((win window))
  (with-slots (lines topline buffer) win
    (dotimes (x (- lines 3))
      (scroll-up win))
    (set-dot buffer topline 0)))

(defmethod update-window-cursor-location ((win window))
  (with-slots (buffer lines cols curline curcol
	       wincurline wincurcol line-offset-data
	       topline) win
    (let* ((rline (- curline topline)))
      (setf wincurline (nth rline line-offset-data))
      (when (null wincurline)
	(setf wincurline (car (last line-offset-data))))
      (if (>= curcol cols)
	  (setf wincurline (+ (floor (/ curcol cols)) wincurline)
		wincurcol (mod curcol cols))
	  (setf wincurcol curcol)))))

(defmethod win-cursor-up ((win window))
  (with-slots (curline curcol topline lines buffer cols shown-lines) win
    (let ((curdot nil))
      (sendcmd buffer :cursor-up)
      (setf curdot (getrval (sendcmd buffer :get-cursor)))
      (setf curline (car curdot)
	    curcol (cadr curdot))
      (when (< curline topline)
	(scroll-up win)))))

(defmethod win-cursor-down ((win window))
  (with-slots (curline curcol topline lines buffer shown-lines) win
    (sendcmd buffer :cursor-down)
    (let ((curdot (getrval (sendcmd buffer :get-cursor))))
      (setf curline (car curdot)
	    curcol (cadr curdot))
      (when (>= curline (+ topline shown-lines))
	(scroll-down win)))))

(defmethod get-win-update ((win window))
  (with-slots (buffer topline curline curcol lines
	       wind-data wincurline wincurcol
	       hasmark markline) win
    (update-window-data win)
    (when (< curline topline)
      (dotimes (x (- topline curline))
	(scroll-up win))
      ;;(sendcmd buffer :set-cursor topline curcol)
      (update-window-data win))
    (when (>= curline (+ topline lines))
      (dotimes (x (- (1+ curline) (+ topline lines)))
	(scroll-down win))
      ;;(sendcmd buffer :set-cursor (+ (1- topline) lines) curcol)
      (update-window-data win))
    (format-window-data win)
    (update-window-cursor-location win)
    `((:dot (,wincurline ,wincurcol))
      (:attrib ,(if (and hasmark (< markline topline))
		    :reverse
		    :no-reverse))
      (:lines ,(if (> (length wind-data) lines)
		   (subseq wind-data 0 lines)
		   wind-data)))))

(defmethod window-resize ((win window) nlines ncols)
  (with-slots (lines cols) win
    (setf lines nlines
	  cols ncols)))

(defmethod change-buffer ((win window) (buf buffer))
  (with-slots (buffer buf-data) win
    (unless (null buffer)
      (sendcmd buffer :hidden)
      (sendcmd buffer :unset-owner))
    (setf buffer buf)
    (setf buf-data nil)
    (sendcmd buffer :visible)
    (sendcmd buffer :set-owner win))
  t)

;;;;;; command handling for windows ;;;;;;

(defmacro defcmd-win (name fun &optional (nargs nil))
  `(defcommand *window-cmd-template*
     ,name
     ,fun
     :nargs ,nargs
     :object t))

(defcmd-win :scroll-up #'scroll-up)
(defcmd-win :scroll-down #'scroll-down)
(defcmd-win :cursor-up #'win-cursor-up)
(defcmd-win :cursor-down #'win-cursor-down)
(defcmd-win :window-update #'get-win-update)
(defcmd-win :window-resize #'window-resize 2)
(defcmd-win :page-up #'page-up)
(defcmd-win :page-down #'page-down)
(defcmd-win :change-buffer #'change-buffer 1)

(defmethod proc-entry ((win window))
  (with-slots (buffer) win
    (labels ((default-function (msgdata)
	       (let ((rval (message buffer msgdata)))
		 (if (null (cadr rval))
		     (list :status :failed-reply :returns nil)
		     (car rval)))))
      ;; step one is to initialize the buffer to something sane
      (sendcmd buffer :set-cursor 0 0)
      (run-table win win
		 :default-function #'default-function
		 :end-command :end-command)
      ;; when the table is done, unset the buffer owner
      (sendcmd buffer :unset-owner)
      (sendcmd buffer :hidden)
      nil)))

(defmethod proc-exit ((win window))
  (when (message-in-flight-p win)
    (reply (get-in-flight-message win)
	   (list :status :process-error :returns nil)
	   :blockp nil))
  (alert-reaper win))

(defun make-window (name buffer lines cols &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let ((newwin (make-instance 'window :name name
				       :buffer buffer
				       :lines lines
				       :cols cols)))
    (add-template-to-cmd-table newwin *window-cmd-template* newwin)
    (start-process newwin)
    (noreturn-sendcmd buffer :set-owner newwin) ;; be sure to tell the buffer who's problem it is
    (noreturn-sendcmd buffer :visible)
    newwin))
