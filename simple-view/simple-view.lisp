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
  ;;(setf *buffer* (make-buffer 'basic-buffer "main buffer"))
  (setf *buffer* (make-buffer 'file-buffer "main buffer" :file "README.org"))
  (setf *window* (make-window "main window" *buffer* 23 80))
  (unwind-protect
       (let* ((update-data nil)
	      (cursor-line 0)
	      (cursor-col 0)
	      (lines nil)
	      (draw-line 0)
	      (draw-col 0)
	      (cur-dot nil)
	      (cursz-lines 23)
	      (cursz-cols 80)
	      ;;(start-attrib '(:no-reverse))
	      (attr-reverse #x40000)
	      (cursor-up (get-code-by-name "up"))
	      (cursor-down (get-code-by-name "down"))
	      (cursor-left (get-code-by-name "left"))
	      (cursor-right (get-code-by-name "right"))
	      (backspace (get-code-by-name "backspace"))
	      (enter (get-code-by-name "C-j"))
	      (space (get-code-by-name "space"))
	      (c nil)
	      (clipboard nil)
	      )
	 (charms:with-curses ()
	   (charms:disable-echoing)
	   (charms:enable-raw-input :interpret-control-characters nil)
	   ;;;(charms:enable-non-blocking-mode charms:*standard-window*)
	   (charms:disable-non-blocking-mode charms:*standard-window*)
	   (charms:clear-window charms:*standard-window*)
	   (loop named main-loop
		 do (progn
		      ;; process the input
		      (cond
			((null c) nil)
			((equal c cursor-up) (sendcmd *window* :cursor-up))
			((equal c cursor-down) (sendcmd *window* :cursor-down))
			((equal c cursor-left) (sendcmd *window* :cursor-left))
			((equal c cursor-right) (sendcmd *window* :cursor-right))
			((equal c enter) (sendcmd *window* :newline))
			((equal c space) (sendcmd *window* :space))
			((equal c backspace) (sendcmd *window* :backspace))
			((equal c (kbd "C-a")) (sendcmd *window* :set-mark))
			((equal c (kbd "C-s")) (sendcmd *window* :unset-mark))
			((equal c (kbd "C-x")) (return-from main-loop))
			((equal c (kbd "C-c")) (setf clipboard (getf (sendcmd *window* :cut) :returns)))
			((equal c (kbd "C-v")) (unless (null clipboard)
						 (sendcmd *window* :paste clipboard)))
			((equal c (kbd "pageup")) (sendcmd *window* :page-up))
			((equal c (kbd "pagedown")) (sendcmd *window* :page-down))
			(t (sendcmd *window* :insert (code-char c))))
		      ;; get window update
		      (multiple-value-bind (ncols nlines)
			  (charms:window-dimensions charms:*standard-window*)
			(when (or (not (= ncols cursz-cols))
				  (not (= nlines cursz-lines)))
			  (setf cursz-lines nlines
				cursz-cols ncols)
			  (sendcmd *window* :window-resize (- cursz-lines 1) (- cursz-cols 0))))
		      (setf update-data (cled-core::getrval (sendcmd *window* :window-update)))
		      (setf cur-dot (cled-core::getrval (sendcmd *window* :get-cursor)))
		      (setf cursor-line (car (cadr (assoc :dot update-data)))
			    cursor-col (cadr (cadr (assoc :dot update-data)))
			    lines (cadr (assoc :lines update-data))
			    ;;start-attrib (cdr (assoc :attrib update-data))
			    )
		      ;; clear the window
		      ;;(charms:refresh-window charms:*standard-window*)
		      (charms:clear-window charms:*standard-window*)
		      ;; properly update the display
		      (charms:with-restored-cursor charms:*standard-window*
			(dolist (line lines)
			  (dolist (cc line)
			    (if (typep cc 'keyword)
				(progn nil)
				(progn
				  (setf *last-print* cc
					*last-print-type* (type-of cc))
				  (charms:write-char-at-point charms:*standard-window* cc draw-col draw-line)
				  (incf draw-col))))
			  (setf draw-col 0)
			  (incf draw-line)))
		      (charms/ll:attron attr-reverse)
		      (charms:write-string-at-point
		       charms:*standard-window*
		       (format nil "cursor = (~A, ~A), dot = (~A, ~A). C-x to quit"
			       cursor-line cursor-col
			       (car cur-dot) (cadr cur-dot))
		       1 (1- cursz-lines))
		      (charms/ll:attroff attr-reverse)
		      (setf draw-col 0
			    draw-line 0)
		      (charms:move-cursor charms:*standard-window* cursor-col cursor-line)
		      ;; get the next key
		      (setf c (get-canonical-key))
		      ;;(sleep 0.005)
		      )
		 )))
    (stop-process *window*)
    (stop-process *buffer*)
    (stop-reaper)))
