;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

;;; a lot of this basic design and all of the codes are cribbed from
;;; lem. 

(in-package :cled)

(defvar *buffer* nil)
(defvar *window* nil)
(defvar *last-print* nil)
(defvar *last-print-type* nil)

(defmacro dprint (stream string &rest forms)
  `(progn
     (format ,stream ,string ,@forms)
     (force-output ,stream)))

(defun simple-viewer ()
  (start-grim-reaper)
  (setf *buffer* (make-buffer 'simple-buffer "main buffer"))
  (setf *window* (make-window "main window" *buffer* 23 80))
  (unwind-protect
       (let* ((update-data nil)
	      (cursor-line 0)
	      (cursor-col 0)
	      (lines nil)
	      (draw-line 0)
	      (draw-col 0)
	      (cursor-up (get-code-by-name "up"))
	      (cursor-down (get-code-by-name "down"))
	      (cursor-left (get-code-by-name "left"))
	      (cursor-right (get-code-by-name "right"))
	      (backspace (get-code-by-name "backspace"))
	      (enter (get-code-by-name "C-j"))
	      (space (get-code-by-name "space"))
	      )
	 (dprint t "checkpoint 1~%")
	 (charms:with-curses ()
	   (charms:disable-echoing)
	   (charms:enable-raw-input :interpret-control-characters t)
	   (charms:enable-non-blocking-mode charms:*standard-window*)
	   (charms:clear-window charms:*standard-window*)
	   (loop named main-loop
		 for c = (get-key (lambda () (charms:get-char charms:*standard-window* :ignore-error t)))
		 do (progn
		      ;; get window update
		      (setf update-data (cled-core::getrval (sendcmd *window* :window-update)))
		      (setf cursor-line (car (cadr update-data))
			    cursor-col (cadr (cadr update-data))
			    lines (caddr update-data))
		      ;; clear the window
		      (charms:refresh-window charms:*standard-window*)
		      ;; properly update the display
		      (charms:with-restored-cursor charms:*standard-window*
			(dolist (line lines)
			  (dolist (cc line)
			    (setf *last-print* cc
				  *last-print-type* (type-of cc))
			    (charms:write-char-at-point charms:*standard-window* cc draw-col draw-line)
			    (incf draw-col))
			  (setf draw-col 0)
			  (incf draw-line)))
		      ;; process the input
		   ;;   (unless (null c)
		;;	(format t "[ char = \"~A\" ]" c))
		      (cond
			((null c) nil)
			((equal c cursor-up) (sendcmd *window* :cursor-up))
			((equal c cursor-down) (sendcmd *window* :cursor-down))
			((equal c cursor-left) (sendcmd *window* :cursor-left))
			((equal c cursor-right) (sendcmd *window* :cursor-right))
			((equal c enter) (sendcmd *window* :newline))
			((equal c space) (sendcmd *window* :space))
			((equal c backspace) (sendcmd *window* :backspace))
			(t (sendcmd *window* :insert c)))
		      (charms:write-string-at-point charms:*standard-window*
						    (format nil "draw = (~A, ~A), cursor = (~A, ~A)"
							    draw-line draw-col cursor-line cursor-col)
						    1 23)
		      (setf draw-col 0
			    draw-line 0)
		      (charms:move-cursor charms:*standard-window* (1+ cursor-col) cursor-line)
		      ))))
    (stop-process *window*)
    (stop-process *buffer*)
    (stop-reaper)))
