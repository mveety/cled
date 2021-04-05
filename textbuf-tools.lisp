;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defun make-textbuf (list)
  "This will make a new textbuffer from a list of strings"
  (let ((new-tbuf (make-instance 'textbuf))
		(iter 1)
		(tmplist nil))
	(dolist (l list)
	  (set-dot new-tbuf iter 0)
	  (setf tmplist (coerce l 'list))
	  (dolist (c tmplist)
		(insert-char new-tbuf c))
	  (insert-line new-tbuf)
	  (incf iter))
	new-tbuf))

(defmethod dl-to-list ((list textbuf))
  "This version dl-to-list will return the list reversed"
  (if (< (dl-length list) 1)
	  nil
	  (let ((tmp nil))
		(dl-head list)
		(loop do
		  (push (dl-data list) tmp)
		  (if (null (dl-next list))
			  (loop-finish)))
		tmp)))

(defun tbuf-to-list (tbuf)
  (let* ((cur-dot (get-dot tbuf))
		 (tmp1 (dl-to-list tbuf))
		 (tmp2 nil))
	(dolist (l tmp1)
	  (push (coerce (dl-to-list l) 'string) tmp2))
	(apply #'set-dot (cons tbuf cur-dot))
	tmp2))

(defun get-n-lines (tbuf start nlines &optional (reverse t))
  (let ((i 0)
		(rlines)
		(cur-dot (get-dot tbuf)))
	(set-dot tbuf start 0)
	(loop do
	  (when (>= i nlines)
		(loop-finish))
	  (push (dl-data tbuf) rlines)
	  (incf i)
	  (when (null (dl-next tbuf))
		(loop-finish)))
	(apply #'set-dot (cons tbuf cur-dot))
	(if reverse
		(nreverse rlines)
		rlines)))

(defun line-list-to-cs (lines-list &key (strings t))
  (let ((ls nil))
	(dolist (l lines-list)
	  (if strings
		  (push (coerce (dl-to-list l) 'string) ls)
		  (push (dl-to-list l) ls)))
	ls))

(defmacro line-list-to-strings (lines-list)
  `(line-list-to-cs ,lines-list :strings t))

(defmacro line-list-to-chars (lines-list)
  `(line-list-to-cs ,lines-list :strings nil))

(defun merge-lines (tbuf source-line)
  "This will merge a line with the line above it"
  (if (<= (car (get-dot tbuf)) 1)
	  nil
	  (progn
		(set-dot tbuf source-line 0)
		(let ((source (dl-data tbuf))
			  (dest nil)
			  (dest-col-dot 0))
		  ; nuke the source line
		  (remove-line tbuf)
		  ; current line is now the dot
		  (setf dest (dl-data tbuf))
		  (dl-tail dest)
		  (setf dest-col-dot (line-dot dest))
		  (dl-append-list dest (dl-to-list source))
		  (set-line-dot dest dest-col-dot)))))
