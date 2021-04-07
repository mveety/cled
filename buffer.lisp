;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defclass simple-buffer (textbuf port command-table)
  ((type :initform 'simple) ;; the buffer type
   (name :initform "unnamed" :initargs :name)
   (lock :initform nil :initargs :lock)
   (dirty :initform t) ;; has the buffer changed since last update
   (no-update-if-clean :initform t) ;; if set, only do a full update if dirty
   (owning-window :initform nil) ;; the window that owns the buffer
   (visible :initform nil) ;; is the buffer visible
   (thread :initform nil :initarg thread)))

;;; buffers are interfaced with much like how a text file would be interacted with
;;; in a text editor. Simple-buffers are extremely simple and only have the commands
;;; to do basic cursor movement, editing, and process management.

;; buffer info and control
(defgeneric buffer-dirty-p (buf)) ;; :dirty -> t if dirty
(defgeneric set-buffer-dirty (buf)) ;; :set-dirty -> nil
(defgeneric unset-buffer-dirty (buf)) ;; :unset-dirty -> nil
(defgeneric get-buffer-update (buf start length &key full initial)) ;; :get-update [args] -> update...
(defgeneric set-buffer-owner (buf win)) ;; :set-owner [owner] -> t,nil
(defgeneric unset-buffer-owner (buf)) ;; :unset-owner -> t,nil
(defgeneric set-name (buf name)) ;; :set-name [name]
(defgeneric show-buffer (buf)) ;; :visible
(defgeneric hide-buffer (buf)) ;; :hidden
(defgeneric buffer-nlines (buf)) ;; :nlines

;;; cursor movement
(defgeneric cursor-up (buf &optional repeat)) ;; :cursor-up [n] -> t,nil
(defgeneric cursor-down (buf &optional repeat)) ;; :cursor-down [n] -> t,nil
(defgeneric cursor-left (buf &optional repeat)) ;; :cursor-left [n] -> t,nil
(defgeneric cursor-right (buf &optional repeat)) ;; :cursor-right [n] -> t,nil
(defgeneric set-cursor (buf line col)) ;; :set-cursor [line] [col] -> t,nil
(defgeneric get-cursor (buf)) ;; :get-cursor -> (line col)

;;; text insertion/deletion
(defgeneric buffer-backspace (buf)) ;; :backspace
(defgeneric buffer-insert-char (buf c)) ;; :insert [char]
(defgeneric buffer-tab (buf)) ;; :tab
(defgeneric buffer-newline (buf)) ;; :newline
(defgeneric buffer-space (buf)) ;; :space

;;; really basic text editing
(defgeneric buffer-copy-char (buf)) ;; :char-copy
(defgeneric buffer-cut-char (buf)) ;; :char-cut


(defmethod buffer-dirty-p ((buf simple-buffer))
  (slot-value buf 'dirty))

(defmethod set-buffer-dirty ((buf simple-buffer))
  (setf (slot-value buf 'dirty) t))

(defmethod unset-buffer-dirty ((buf simple-buffer))
  (setf (slot-value buf 'dirty) nil))

(defmethod get-buffer-update ((buf simple-buffer) start length &key (full nil) (initial nil))
  (with-slots (dirty no-update-if-clean) buf
	(let ((update-list nil)
		  (cursor-pos (get-dot buf))
		  (lines nil))
	  (if (and (not dirty)
			   no-update-if-clean
			   (not full)
			   (not initial))
		  (list '(:dot) cursor-pos nil)
		  (list '(:dot :lines)
				cursor-pos
				(line-list-to-chars (get-n-lines buf start length nil)))))))

(defmethod set-buffer-owner ((buf simple-buffer) win)
  (setf (slot-value buf 'owning-window) win))

(defmethod unset-buffer-owner ((buf simple-buffer))
  (setf (slot-value buf 'owning-window) win))

(defmethod show-buffer ((buf simple-buffer))
  (setf (slot-value buf 'visible) t))

(defmethod hide-buffer ((buf simple-buffer))
  (setf (slot-value buf 'visible) nil))

(defmethod buffer-nlines ((buf simple-buffer))
  (dl-length buf))

(defmethod cursor-up ((buf simple-buffer) &optional (repeat 1))
  (let ((cur-dot nil))
	(dotimes (i repeat)
	  (setf cur-dot (get-dot buf))
	  (unless (= (car cur-dot) 1)
		(set-dot buf (1- (car cur-dot)) (cadr cur-dot))))))

(defmethod cursor-down ((buf simple-buffer) &optional (repeat 1))
  (let ((cur-dot nil))
	(dotimes (i repeat)
	  (setf cur-dot (get-dot buf))
	  (unless (= (car cur-dot) (dl-length buf))
		(set-dot buf (1+ (car cur-dot)) (cadr cur-dot))))))

(defmethod cursor-left ((buf simple-buffer) &optional (repeat 1))
  (let ((cur-dot nil))
	(dotimes (i repeat)
	  (setf cur-dot (get-dot buf))
	  (if (= (cadr cur-dot) 1)
		  (progn
			(unless (= (car cur-dot) 1)
			  (cursor-up buf)
			  (set-dot buf (car (get-dot buf)) (line-length buf))))
		  (set-dot buf (car cur-dot) (1- (cadr cur-dot)))))))

(defmethod cursor-right ((buf simple-buffer) &optional (repeat 1))
  (let ((cur-dot nil))
	(dotimes (i repeat)
	  (setf cur-dot (get-dot buf))
	  (if (= (cadr cur-dot) (line-length buf))
		  (progn
			(unless (= (car cur-dot) (dl-length buf))
			  (cursor-down buf)
			  (set-dot buf (car (get-dot buf)) 1)))
		  (set-dot buf (car cur-dot) (1+ (cadr cur-dot)))))))

(defmethod set-cursor ((buf simple-buffer) line col)
  (set-dot buf line col))

(defmethod get-cursor ((buf simple-buffer))
  (get-dot buf))
