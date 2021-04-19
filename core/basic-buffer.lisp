;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass basic-buffer (simple-buffer)
  ((type :initform 'basic-buffer)
   (type-string :initform "basic-buffer")
   (mark :initform nil)))

(defvar *basic-buffer-cmd-template* nil
  "Command table template for basic-buffers")

(defgeneric bb-set-mark (buf))
(defgeneric bb-cut-and-copy (buf copy))
(defgeneric bb-cut (buf))
(defgeneric bb-copy (buf))
(defgeneric bb-paste (buf data))

(defmethod bb-set-mark ((buf basic-buffer))
  (with-slots (mark) buf
    (setf mark (get-dot buf))))

(defun bb-cut-and-copy-line (buf start finish copy)
  (set-buffer-dirty buf)
  (when (> finish (line-length buf))
    (setf finish (line-length buf)))
  (if (= start finish)
      nil
      (let ((data nil)
	    (cur-dot (get-dot buf)))
	(set-dot buf (car cur-dot) start)
	(dotimes (x (- finish start))
	  (push (get-char buf) data)
	  (set-dot buf (car cur-dot) (1+ start))
	  (if copy
	      (incf start)
	      (remove-char buf)))
	(reverse data))))

(defmethod bb-cut-and-copy ((buf basic-buffer) copy)
  (set-buffer-dirty buf)
  (with-slots (mark) buf
    (let ((cur-dot (get-dot buf))
	  (line-data nil)
	  (data nil)
	  (nlines 0)
	  (first-line-full nil))
      (if (= (car mark) (car cur-dot))
	  (push (bb-cut-and-copy-line buf (cadr mark) (cadr cur-dot) copy) data)
	  (progn
	    (set-dot buf (car mark) (cadr mark))
	    (setf nlines (- (car mark) (car cur-dot)))
	    (dotimes (x (1- nlines))
	      (if (= x 0)
		  (progn
		    (setf line-data (bb-cut-and-copy-line buf (cadr mark) (line-length buf) copy))
		    (unless (= (line-length buf) 0)
		      (setf first-line-full t)))
		  (setf line-data (bb-cut-and-copy-line buf 0 (line-length buf) copy)))
	      (push line-data data)
	      (when (= (line-length buf) 0)
		(remove-line buf))
	      (dl-next buf))
	    (setf line-data (bb-cut-and-copy-line buf 0 (cadr cur-dot) copy))
	    (push line-data data)
	    (if (= (line-length buf) 0)
		(remove-line buf)
		(when (and first-line-full (null copy))
		  (merge-lines buf (car (get-dot buf)))))
	    (setf data (reverse data))))
      (if copy
	  (set-dot buf (car cur-dot) (cadr cur-dot))
	  (set-dot buf (car mark) (cadr mark)))
      (reverse data))))

(defmethod bb-cut ((buf basic-buffer))
  (bb-cut-and-copy buf nil))

(defmethod bb-copy ((buf basic-buffer))
  (bb-cut-and-copy buf t))
