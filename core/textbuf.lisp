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
	((>= new-loc length) (dl-tail list) nil)
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
(defgeneric zero-dot (tbuf))
(defgeneric (setf zero-dot) (data tbuf))
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
(defgeneric line-length (tbuf))

(defmethod initialize-instance :after ((tbuf textbuf) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (dl-push tbuf (make-instance 'tb-line)))

(defmethod zero-dot ((tbuf textbuf))
  (slot-value (dl-data tbuf) 'zero-dot))

(defmethod (setf zero-dot) (data (tbuf textbuf))
  (setf (slot-value (dl-data tbuf) 'zero-dot) data))

(defmethod linen ((tbuf textbuf))
  (1+ (dl-loc tbuf)))

(defmethod set-linen ((tbuf textbuf) n)
  (move-to tbuf (1- n)))

(defmethod set-dot ((tbuf textbuf) linen coln)
  (list
   (set-linen tbuf linen)
   (set-line-dot (dl-data tbuf) coln)))

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
    (prog1
	(if (= (dl-length line) 0)
	    nil
	    (dl-remove line))
      (when (= (dl-length line) 0)
	(setf (slot-value line 'zero-dot) t))
	)))

(defmethod get-char ((tbuf textbuf))
  (dl-data (dl-data tbuf)))

(defmethod set-char ((tbuf textbuf) char)
  (setf (dl-data (dl-data tbuf)) char))

(defmethod get-line ((tbuf textbuf))
  (dl-to-list (dl-data tbuf)))

(defmethod line-length ((tbuf textbuf))
  (dl-length (dl-data tbuf)))

(defmethod print-object ((list textbuf) stream)
  (print-unreadable-object (list stream :type t)
    (let ((tmplst (dlist::copy-dlist list))
	  (counter 0)
	  (curdot (get-dot list)))
      (if (> (dl-length tmplst) 0)
	  (progn
	    (format stream "length: ~A, dot: (~A,~A), data: ("
		    (dl-length tmplst)
		    (car curdot)
		    (cadr curdot))
	    (loop do
	      (format stream "~S" (dl-data tmplst))
	      (if (null (dl-next tmplst))
		  (loop-finish)
		  (if (< counter (1- *element-printing-limit*))
		      (format stream " ")
		      (progn
			(format stream "...")
			(loop-finish))))
	      (incf counter))
	    (format stream ")"))
	  (format stream "length: 0, data: nil")))))

(defmethod print-object ((list tb-line) stream)
  (print-unreadable-object (list stream :type t)
    (let ((tmplst (dlist::copy-dlist list))
	  (counter 0))
      (if (> (dl-length tmplst) 0)
	  (progn
	    (format stream "length: ~A, zero-dot: ~A, data: "
		    (dl-length tmplst)
		    (slot-value list 'zero-dot))
	    (loop do
	      (format stream "~S" (dl-data tmplst))
	      (if (null (dl-next tmplst))
		  (loop-finish)
		  (if (< counter (1- *element-printing-limit*))
		      (format stream " ")
		      (progn
			(format stream "...")
			(loop-finish))))
	      (incf counter)))
	  (format stream "length: 0, zero-dot: ~A, data: nil" (slot-value list 'zero-dot))))))
