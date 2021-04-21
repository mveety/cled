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
(defgeneric bb-unset-mark (buf))
(defgeneric bb-get-mark (buf))
(defgeneric bb-cut-and-copy (buf copy))
(defgeneric bb-cut (buf))
(defgeneric bb-copy (buf))
(defgeneric bb-paste (buf data))

(defmethod bb-set-mark ((buf basic-buffer))
  (with-slots (mark) buf
    (setf mark (get-dot buf))))

(defmethod bb-unset-mark ((buf basic-buffer))
  (with-slots (mark) buf
    (setf mark nil)))

(defmethod bb-get-mark ((buf basic-buffer))
  (slot-value buf 'mark))

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

(defun dots-greater-than (dot-a dot-b)
  (if (> (car dot-a) (car dot-b))
      t
      (if (= (car dot-a) (car dot-b))
	  (if (> (cadr dot-a) (cadr dot-b))
	      t
	      nil)
	  nil)))

(defmethod bb-cut-and-copy ((buf basic-buffer) copy)
  (set-buffer-dirty buf)
  (with-slots (mark) buf
    (let ((cur-dot (get-dot buf))
	  (line-data nil)
	  (data nil)
	  (nlines 0)
	  (last-line-full nil)
	  (first-line-full nil))
      (when (dots-greater-than mark cur-dot)
	(setf cur-dot mark
	      mark (get-dot buf)))
      (if (= (car mark) (car cur-dot))
	  (push (bb-cut-and-copy-line buf (cadr mark) (cadr cur-dot) copy) data)
	  (progn
	    (set-dot buf (car mark) (cadr mark))
	    (setf nlines (- (car cur-dot) (car mark)))
	    (dotimes (x nlines)
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
	    (when (= (cadr cur-dot) (line-length buf))
	      (setf last-line-full t))
	    (setf line-data (bb-cut-and-copy-line buf 0 (cadr cur-dot) copy))
	    (push line-data data)
	    (if (= (line-length buf) 0)
		(remove-line buf)
		(when (and first-line-full (null copy))
		  (merge-lines buf (car (get-dot buf)))))
	    (when last-line-full
	      (push nil data))
	    (setf data (reverse data))))
      (if copy
	  (set-dot buf (car cur-dot) (cadr cur-dot))
	  (set-dot buf (car mark) (cadr mark)))
      data)))

(defmethod bb-cut ((buf basic-buffer))
  (bb-cut-and-copy buf nil))

(defmethod bb-copy ((buf basic-buffer))
  (bb-cut-and-copy buf t))

(defmethod bb-paste ((buf basic-buffer) data)
  (set-buffer-dirty buf)
  (let ((dot (get-dot buf)))
    (cond
      ((= (cadr dot) 0)
       (insert-line buf :above t)
       (set-dot buf (1- (car (get-dot buf))) 0)
       (dolist (l data)
	 (dolist (c l)
	   (insert-char buf c))
	 (insert-line buf)
	 (set-dot buf (1+ (car (get-dot buf))) 0))
       (remove-line buf)
       (merge-lines buf (1+ (car (get-dot buf)))))
      ((= (cadr dot) (line-length buf))
       (insert-line buf)
       (set-dot buf (1+ (car (get-dot buf))) 0)
       (dolist (l data)
	 (dolist (c l)
	   (insert-char buf c))
	 (insert-line buf)
	 (set-dot buf (1+ (car (get-dot buf))) 0))
       (remove-line buf))
      (t
       (split-line buf (car dot) (cadr dot))
       (dolist (l data)
	 (dolist (c l)
	   (insert-char buf c))
	 (insert-line buf)
	 (set-dot buf (1+ (car (get-dot buf))) 0))
       (remove-line buf)
       (set-dot buf (1+ (car (get-dot buf))) 0)
       (merge-lines buf (car (get-dot buf)))))))
       
(defmacro defcmd-bbuf (name fun &optional (nargs nil))
  `(defcommand *basic-buffer-cmd-template*
     ,name
     ,fun
     :nargs ,nargs
     :object t))

(defcmd-bbuf :set-mark #'bb-set-mark)
(defcmd-bbuf :unset-mark #'bb-unset-mark)
(defcmd-bbuf :get-mark #'bb-get-mark)
(defcmd-bbuf :copy #'bb-copy)
(defcmd-bbuf :cut #'bb-cut)
(defcmd-bbuf :paste #'bb-paste 1)

(defmethod initialize-instance :after ((buf basic-buffer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (add-template-to-cmd-table buf *basic-buffer-cmd-template* buf))

(defun make-basic-buffer (name &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let ((newbuf (make-instance 'basic-buffer :name name)))
    (start-process newbuf)
    newbuf))

(define-buffer-type 'basic-buffer #'make-basic-buffer)
