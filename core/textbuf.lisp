;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass tb-line (dlist)
  ((at-end :initform t)
   )
  (:documentation "A line of text"))

(defgeneric move-to (list new-loc))

(defmethod move-to ((list dlist) new-loc)
  (with-slots (length (loc dlist::loc)) list
    (cond
      ((= new-loc 0) (dl-head list))
      ((>= new-loc length) (dl-tail list) nil)
      (t (let ((roffset (- new-loc loc)))
	   (cond
	     ((> roffset 0) (dl-nextn list roffset))
	     ((< roffset 0) (dl-prevn list (abs roffset)))
	     ((= roffset 0) t)))))))

(defgeneric set-line-dot (line new-dot))
(defgeneric line-dot (line))
(defgeneric insert-at-line-dot (line char))

(defmethod set-line-dot ((line tb-line) new-dot)
  (setf (slot-value line 'at-end) nil)
  (if (= (dl-length line) 0)
      (progn
	(setf (slot-value line 'at-end) t)
	t)
      (if (<= new-dot 0)
	  (progn
	    (move-to line 0))
	  (if (>= new-dot (dl-length line))
	      (progn
		(dl-tail line)
		(setf (slot-value line 'at-end) t))
	      (progn
		(move-to line new-dot)
		(setf (slot-value line 'at-end) nil))))))

(defmethod line-dot ((line tb-line))
  (if (slot-value line 'at-end)
      (if (= (dl-length line) 0)
	  0
	  (1+ (dl-loc line)))
      (dl-loc line)))

(defmethod insert-at-line-dot ((line tb-line) char)
  ;; this is the bomb
  (if (slot-value line 'at-end)
      (progn
	(dl-append line char)
	(dl-next line))
      (dl-insert-after line char)))

(defmethod remove-at-line-dot ((line tb-line))
  (if (slot-value line 'at-end)
      (dl-remove line)
      (progn
	(if (= (dl-loc line) 0)
	    nil ;; do nothing at the head of a line
	    (let ((old-loc (dl-loc line)))
	      (dl-prev line)
	      (dl-remove line)
	      (unless (= old-loc 1)
		(dl-next line))))
	(when (= (dl-length line) 0)
	  (setf (slot-value line 'at-end) t)))))

(defclass textbuf (dlist)
  ((line-dot :initform 0 :initarg :line-dot) ;; 0 <= line-dot < textbuf-length
   (col-dot :initform 0 :initarg :col-dot))) ;; 0 <= col-dot <= line-length
;;; if col-dot == line-length then at-end should be set and loc should be col-dot - 1.

(defgeneric linen (tbuf))
(defgeneric set-linen (tbuf n))
(defgeneric set-dot (tbuf linen coln))
(defgeneric get-dot (tbuf))
(defgeneric insert-line (tbuf &key above tb-line))
(defgeneric remove-line (tbuf))
(defgeneric tb-nuke (tbuf))
(defgeneric insert-char (tbuf char))
(defgeneric remove-char (tbuf))
(defgeneric get-char (tbuf))
(defgeneric set-char (tbuf char))
(defgeneric get-line (tbuf))
(defgeneric line-length (tbuf))
(defgeneric tbuf-length (tbuf))

(defmethod initialize-instance :after ((tbuf textbuf) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (dl-push tbuf (make-instance 'tb-line)))

(defmethod linen ((tbuf textbuf))
  (dl-loc tbuf))

(defmethod set-linen ((tbuf textbuf) n)
  (move-to tbuf n))

(defmethod set-dot ((tbuf textbuf) linen coln)
  (list (set-linen tbuf linen)
	(set-line-dot (dl-data tbuf) coln)))

(defmethod get-dot ((tbuf textbuf))
  (list (linen tbuf)
	(line-dot (dl-data tbuf))))

(defmethod insert-line ((tbuf textbuf) &key (above nil) (tb-line nil))
  (let ((newline (make-instance 'tb-line)))
    (if above
	(dl-insert-after tbuf newline)
	(progn
	  (dl-insert tbuf newline)
	  (dl-prev tbuf)))
    (if tb-line
	newline
	(get-dot tbuf))))

(defmethod remove-line ((tbuf textbuf))
  (dl-remove tbuf)
  (when (<= (dl-length tbuf) 0)
    (dl-insert tbuf (make-instance 'tb-line)))
  (get-dot tbuf))

(defmethod tb-nuke ((tbuf textbuf))
  (dl-nuke tbuf)
  (dl-insert tbuf (make-instance 'tb-line)))

(defmethod insert-char ((tbuf textbuf) char)
  (insert-at-line-dot (dl-data tbuf) char))

(defmethod remove-char ((tbuf textbuf))
  (remove-at-line-dot (dl-data tbuf)))

(defmethod get-char ((tbuf textbuf))
  (dl-data (dl-data tbuf)))

(defmethod set-char ((tbuf textbuf) char)
  (setf (dl-data (dl-data tbuf)) char))

(defmethod get-line ((tbuf textbuf))
  (dl-to-list (dl-data tbuf)))

(defmethod line-length ((tbuf textbuf))
  (dl-length (dl-data tbuf)))

(defmethod tbuf-length ((tbuf textbuf))
  (dl-length tbuf))

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
  (print-unreadable-object (list stream :type t :identity t)
    (let ((tmplst (dlist::copy-dlist list))
	  (counter 0))
      (if (> (dl-length tmplst) 0)
	  (progn
	    (format stream "length: ~A, at-end: ~A, dot: ~A, data: "
		    (dl-length tmplst)
		    (slot-value list 'at-end)
		    (line-dot list))
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
	  (format stream "length: 0, at-end: ~A, dot: 0, data: nil" (slot-value list 'at-end))))))

(defmethod dl-to-list ((list textbuf))
  "This version of dl-to-list will return the list reversed"
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
  (let ((cur-dot (get-dot tbuf)))
    (set-dot tbuf source-line 0)
    (if (<= (car (get-dot tbuf)) 0)
	(progn
	  (apply #'set-dot (cons tbuf cur-dot))
	  nil)
	(let ((source (dl-data tbuf))
	      (dest nil)
	      (dest-col-dot 0))
	  ;; nuke the source line
	  (remove-line tbuf)
	  (setf cur-dot (get-dot tbuf))
	  ;; current line is now the dot
	  (setf dest (dl-data tbuf))
	  (dl-tail dest)
	  (setf dest-col-dot (dl-length dest))
	  (dl-append-list dest (dl-to-list source))
	  (set-line-dot dest dest-col-dot)
	  (apply #'set-dot (list tbuf (car cur-dot) dest-col-dot))))))

(defun split-line (tbuf source-line split-pos)
  (let ((cur-dot (get-dot tbuf))
	(src nil)
	(nlb1 nil)
	(nlb2 nil)
	(nl1 nil)
	(nl2 nil)
	)
    (set-dot tbuf source-line 0)
    (if (and (<= split-pos (line-length tbuf))
	     (= (car (get-dot tbuf)) source-line))
	(progn
	  (setf src (dl-to-list (dl-data tbuf)))
	  (setf nlb2 (insert-line tbuf :tb-line t)
		nlb1 (insert-line tbuf :tb-line t)
		nl1 (subseq src 0 split-pos)
		nl2 (subseq src split-pos (length src)))
	  (dl-append-list nlb1 nl1)
	  (dl-append-list nlb2 nl2)
	  (remove-line tbuf)
	  (apply #'set-dot (cons tbuf cur-dot))
	  t)
	(progn
	  (apply #'set-dot (cons tbuf cur-dot))
	  nil))))

(defun append-list-to-tbuf (tbuf list &key (start 0))
  (let ((iter start))
    (dolist (l list)
      (set-dot tbuf iter 0)
      (dolist (c (coerce l 'list))
	(insert-char tbuf c))
      (insert-line tbuf)
      (incf iter))
    iter))

(defun make-textbuf (list)
  "This will make a new textbuffer from a list of strings"
  (let ((new-tbuf (make-instance 'textbuf))
	(iter 0)
	(tmplist nil))
    (dolist (l list)
      (set-dot new-tbuf iter 0)
      (setf tmplist (coerce l 'list))
      (dolist (c tmplist)
	(insert-char new-tbuf c))
      (insert-line new-tbuf)
      (incf iter))
    new-tbuf))

(defun make-testlist (nlines)
  "This is a cute little secret"
  (let ((dat nil))
    (dotimes (i nlines)
      (push (format nil "@@@@@ This is testline number ~A. TEST TEST TEST @@@@@" i) dat))
    (nreverse dat)))
