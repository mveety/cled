;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass file-buffer (basic-buffer)
  ((type :initform 'file-buffer)
   (type-string :initform "file-buffer")
   (changed :initform nil)
   (filename :initform nil :initarg :file)))

(defvar *file-buffer-cmd-template* nil
  "Command table template for file-buffers")

(defgeneric file-read (buf))
(defgeneric file-write (buf))

(defmethod file-write ((buf file-buffer))
  (with-slots (filename changed) buf
    (if (null filename)
	:no-open-file
	(if (null changed)
	    nil
	    (let ((output nil)
		  (old-file (concatenate 'string filename ".old")))
	      (when config:*make-backup-files*
		(when (probe-file old-file)
		  (delete-file old-file))
		(rename-file filename old-file))
	      (handler-case
		  (with-open-file (outstream filename
					     :direction :output
					     :if-exists :supersede
					     :if-does-not-exist :create)
		    (setf output (tbuf-to-list buf))
		    (dolist (l output)
		      (write-string l outstream)
		      (terpri outstream))
		    (setf changed nil)
		    t)
		(error () :file-error)))))))

(defmethod file-read ((buf file-buffer))
  (with-slots (filename changed) buf
    (if (null filename)
	:no-open-file
	(handler-case
	    (let ((new-tbuf (make-instance 'textbuf))
		  (cur-dot (get-dot buf)))
	      (with-open-file (instream filename
					:direction :input
					:if-does-not-exist :error)
		(loop for tstr = (read-line instream nil nil)
		      until (null tstr)
		      do 
			 (let ((tb-cur-dot (get-dot new-tbuf)))
			   (dolist (c (coerce tstr 'list))
			     (insert-char new-tbuf c))
			   (insert-line new-tbuf)
			   (set-dot new-tbuf (1+ (car tb-cur-dot)) 0))))
	      (dlist::ref-dlist buf new-tbuf) ;; secret squirrel api.
	      (set-dot buf (car cur-dot) (cdr cur-dot))
	      t)
	  (error () :file-error)))))

(defmethod set-buffer-dirty :after ((buf file-buffer))
  (setf (slot-value buf 'changed) t))

(defmacro defcmd-file (name fun &optional (nargs nil))
  `(defcommand *file-buffer-cmd-template*
     ,name
     ,fun
     :nargs ,nargs
     :object t))

(defcmd-file :read-file #'file-read)
(defcmd-file :file-write #'file-write)

(defmethod initialize-instance :after ((buf file-buffer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (add-template-to-cmd-table buf *file-buffer-cmd-template* buf)
  (unless (null (slot-value buf 'filename))
    (file-read buf)))

(defun make-file-buffer (name &rest args &key (file nil) &allow-other-keys)
  (declare (ignore args))
  (let ((newbuf (make-instance 'file-buffer :name name :file file)))
    (start-process newbuf)
    newbuf))

(define-buffer-type 'file-buffer #'make-file-buffer)
