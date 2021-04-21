;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defgeneric bb-search (buffer pattern &key reverse))

(defun tb-line-search (tb-line pattern)
  (if (= (dl-length tb-line) 0)
      nil
      (let ((str (coerce (dl-to-list tb-line) 'string)))
	(search pattern str))))

(defun bb-forward-search (buffer pattern start-dot)
  (let ((left (- (dl-length buffer) (car start-dot)))
	(cur nil)
	(rval nil)
	(curline 0))
    (loop for i from 0 below left
	  do (progn
	       (set-dot buffer (+ (car start-dot) i) (cadr start-dot))
	       (setf cur (dl-data buffer))
	       (when (null cur)
		 (loop-finish))
	       (setf rval (tb-line-search cur pattern))
	       (when (not (null rval))
		 (setf curline (+ (car start-dot) i))
		 (loop-finish))))
    (if (null rval)
	rval
	`(,curline ,rval))))

(defun bb-reverse-search (buffer pattern start-dot)
  (let ((left (+ 0 (car start-dot)))
	(cur nil)
	(rval nil)
	(curline nil))
    (loop for i from 0 below left
	  do (progn
	       (set-dot buffer (- (car start-dot) i) 0)
	       (setf cur (dl-data buffer))
	       (when (null cur)
		 (loop-finish))
	       (setf rval (tb-line-search cur pattern))
	       (when (not (null rval))
		 (setf curline (- (car start-dot) i))
		 (loop-finish))))
    (if (null rval)
	rval
	`(,curline ,rval))))

(defmethod bb-search ((buffer basic-buffer) pattern &key (reverse nil))
  (let ((cur-dot (get-dot buffer)))
    (prog1
	(if reverse
	    (bb-reverse-search buffer pattern cur-dot)
	    (bb-forward-search buffer pattern cur-dot))
      (apply #'set-dot (cons buffer cur-dot)))))

(defcmd-bbuf :forward-search (lambda (b p) (bb-search b p)) 1)
(defcmd-bbuf :reverse-search (lambda (b p) (bb-search b p :reverse t)) 1)
