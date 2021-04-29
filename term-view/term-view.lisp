;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled)

(defclass term-view (port process)
  ((type :initform 'viewer)
   (type-string :initform "viewer")
   (window :initform nil :initarg :window)
   (update-data :initform nil)
   (nlines :initform 0)
   (ncols :initform 0)
   (cursor-line :initform 0)
   (cursor-col :initform 0)
   (cur-dot :initform '(0 0))
   (clipboard :initform nil)
   (curses-window :initform nil :initarg :curses-window)
   (mode :initform 'normal)
   (mode-char :initform " ")
   (lines :initform nil)
   ))

(defun normal-insert-mode-keys (c mode win)
  (case c
    (#.(kbd "up") (sendcmd win :cursor-up) nil)
    (#.(kbd "down") (sendcmd win :cursor-down) nil)
    (#.(kbd "left") (sendcmd win :cursor-left) nil)
    (#.(kbd "right") (sendcmd win :cursor-right) nil)
    (#.(kbd "pageup") (sendcmd win :page-up) nil)
    (#.(kbd "pagedown") (sendcmd win :page-down) nil)
    (#.(kbd "escape") 'end-mode)
    (t (if (equal mode 'insert)
	   (case c
	     (#.(kbd "enter") (sendcmd win :newline) nil)
	     (#.(kbd "space") (sendcmd win :space) nil)
	     (#.(kbd "rubout") (sendcmd win :backspace) nil)
	     (#.(kbd "backspace") (sendcmd win :backspace) nil)
	     (#.(kbd "tab") (sendcmd win :tab) nil)
	     (t (when (>= c (kbd "space"))
		  (sendcmd win :insert (code-char c))
		  nil)))
	   (progn ;; for the future
	     nil
	     )))))
  
(defmethod proc-entry ((p term-view))
  (with-slots (window curses-window update-data nlines
	       ncols cur-dot clipboard mode cursor-line
	       cursor-col lines mode-char) p
    (charms:clear-window curses-window)
    (let (msg)
      (loop
	named main-loop
	do (progn
	     ;; step 1: wait for and process an event
	     (setf msg (waitformsg p))
	     (with-slots ((payload cled-core::payload)) msg
	       (reply msg (list :status nil :return t))
	       (case (car payload)
		 (:end-command (progn
				 (no-process-restart p)
				 (return-from main-loop)))
		 (:restart (return-from main-loop))
		 (:keypress
		  (let ((mr (case mode
			      ((or normal insert) (normal-insert-mode-keys (cadr payload) mode window))
			      (command (minibuf-mode-keys (cadr payload) mode window)))))
		    (case mr
		      (end-mode (setf mode 'normal
				      mode-char " "))
		      (normal (setf mode 'normal
				    mode-char " "))
		      (insert (setf mode 'insert
				     mode-char "i"))
		      (command (setf mode 'command
				     mode-char "c"))
		      (otherwise nil))))
		 (:resize (progn
			    (setf nlines (cadr payload)
				  ncols (caddr payload))
			    (sendcmd window :window-resize (- nlines 2) ncols)))
		 (:mode (setf mode (cadr payload) ;; this is for debugging. don't use it.
			      mode-char (caddr payload)))
		 (:update nil)
		 (otherwise nil)))
	     ;; step 2: get the window update
	     (unless (or (equal nlines 0)
			 (equal ncols 0))
	       (setf update-data (cled-core::getrval (sendcmd window :window-update))
		     cur-dot (cled-core::getrval (sendcmd window :get-cursor))
		     cursor-line (car (cadr (assoc :dot update-data)))
		     cursor-col (cadr (cadr (assoc :dot update-data)))
		     lines (cadr (assoc :lines update-data)))
	       ;; draw the screen
	       (charms:clear-window curses-window)
	       (charms:with-restored-cursor curses-window
		 (let ((draw-line 0) (draw-col 0))
		   (dolist (line lines)
		     (dolist (char line)
		       (if (typep char 'keyword)
			   (progn ;; for future expansion
			     nil)
			   (progn
			     (charms:write-char-at-point curses-window char draw-col draw-line)
			     (incf draw-col))))
		     (setf draw-col 0)
		     (incf draw-line)))
		 (draw-minibuf curses-window nlines ncols cur-dot mode-char))
	       ;; move the cursor appropriately
	       (charms:move-cursor curses-window cursor-col cursor-line)))))))

(defmethod proc-exit ((p term-view))
  (when (message-in-flight-p p)
    (reply (get-in-flight-message p)
	   '(:status :process-error :returns nil)
	   :blockp nil))
  (alert-reaper p))
