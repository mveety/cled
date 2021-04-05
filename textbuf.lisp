;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass tb-line (dlist)
  ((zero-dot :initform t))
  (:documentation "A line of text"))

(defgeneric move-to (list new-loc)
  (:documentation "Move to a specific location in a line array")
  (:method ((list dlist) new-loc)
	(with-slots (length (loc dlist::loc)) list
	  (cond
		((equal new-loc 0) (dl-head list))
		((>= new-loc length) (dl-tail list))
		(t (let ((roffset (- new-loc loc)))
			 (cond
			   ((> roffset 0) (dl-nextn list roffset))
			   ((< roffset 0) (dl-prevn list (abs roffset)))
			   ((= roffset 0) t))))))))

(defgeneric set-line-dot (line new-dot))
(defgeneric line-dot (line))
(defgeneric insert-at-line-dot (line char))

(defmethod set-line-dot ((line tb-line) new-dot)
  (if (= (dl-length line) 0)
	  t
	  (if (<= new-dot 0)
		  (progn
			(setf (slot-value line 'zero-dot) t)
			(move-to line 0))
		  (progn
			(setf (slot-value line 'zero-dot) nil)
			(move-to line (1- new-dot))))))

(defmethod line-dot ((line tb-line))
  (if (= (dl-loc line) 0)
	  1
	  (1+ (dl-loc line))))

(defmethod insert-at-line-dot ((line tb-line) char)
  (if (or (= (dl-length line) 0)
		  (slot-value line 'zero-dot))
	  (progn
		(setf (slot-value line 'zero-dot) nil)
		(dl-push line char))
	  (dl-insert line char)))

(defclass textbuf (dlist)
  ((line-dot :initform 0 :initarg :line-dot)
   (col-dot :initform 0 :initarg :col-dot)))

(defgeneric linen (tbuf))
(defgeneric set-linen (tbuf n))
(defgeneric set-dot (tbuf linen coln))
(defgeneric get-dot (tbuf))
(defgeneric insert-line (tbuf &key above))
(defgeneric remove-line (tbuf))
(defgeneric insert-char (tbuf char))
(defgeneric remove-char (tbuf))
(defgeneric get-char (tbuf))
(defgeneric set-char (tbuf char))
(defgeneric get-line (tbuf))
(defgeneric cursor-left (tbuf))
(defgeneric cursor-right (tbuf))
(defgeneric cursor-up (tbuf))
(defgeneric cursor-down (tbuf))

(defmethod linen ((tbuf textbuf))
  (1+ (dl-loc tbuf)))

(defmethod set-linen ((tbuf textbuf) n)
  (move-to tbuf (1- n)))

(defmethod set-dot ((tbuf textbuf) linen coln)
  (set-linen tbuf linen)
  (set-line-dot (dl-data tbuf) coln))

(defmethod get-dot ((tbuf textbuf))
  (with-slots (line-dot col-dot) tbuf
	(setf line-dot (linen tbuf)
		  col-dot (line-dot (dl-data tbuf)))
	(list line-dot col-dot)))

(defmethod insert-line ((tbuf textbuf) &key (above nil))
  (if above
	  (if (= (dl-loc tbuf) 0)
		  (dl-push tbuf (make-instance 'tb-line))
		  (progn
			(dl-prev tbuf)
			(dl-insert tbuf (make-instance 'tb-line))
			(dl-next tbuf)))
	  (progn
		(dl-insert tbuf (make-instance 'tb-line))
		(dl-prev tbuf)))
  (get-dot tbuf))

(defmethod remove-line ((tbuf textbuf))
  (dl-remove tbuf)
  (when (= (dl-length tbuf) 0)
	(dl-insert tbuf (make-instance 'tb-line)))
  (get-dot tbuf))

(defmethod insert-char ((tbuf textbuf) char)
  (insert-at-line-dot (dl-data tbuf) char))

(defmethod remove-char ((tbuf textbuf))
  (let ((line (dl-data tbuf)))
	(if (= (dl-length line) 0)
		nil
		(dl-remove line))))

(defmethod get-char ((tbuf textbuf))
  (dl-data (dl-data tbuf)))

(defmethod set-char ((tbuf textbuf) char)
  (setf (dl-data (dl-data tbuf)) char))

(defmethod get-line ((tbuf textbuf))
  (dl-to-list (dl-data tbuf)))
