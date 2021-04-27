;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled)

(defclass cmddef ()
  ((name :initform nil :initarg :name)
   (short :initform nil :initarg :short)
   (documentation :initform nil :initarg :documentation)
   (split-args :initform nil :initarg :split-args)
   (fun :initform nil :initarg :fun)))

(defvar *command-table* (make-hash-table :test 'equal))
(defvar *command-buffer* nil)

(defun find-cmddef (string)
  (let ((cmddef (gethash (subseq string 0 1) *command-table*)))
    (when (null cmddef)
      (let* ((split (split-sequence #\Space string :remove-empty-subseqs t))
	     (cmd (car split)))
	(setf cmddef (gethash cmd *command-table*))))
    cmddef
    ))

(defun run-editor-command (cmddef string)
  (if (slot-value cmddef 'split-args)
      (if (slot-value cmddef 'short)
	  (let* ((trimmed (subseq string 1))
		 (split (split-sequence #\Space trimmed :remove-empty-subseqs t)))
	    (funcall (slot-value cmddef 'fun) split))
	  (let ((lsplit (split-sequence #\Space string :remove-empty-subseqs t)))
	    (funcall (slot-value cmddef 'fun) (cdr lsplit))))
      (if (slot-value cmddef 'short)
	  (funcall (slot-value cmddef 'fun) (subseq string 1))
	  (funcall (slot-value cmddef 'fun) string))))

(defun define-editor-command (name fun &key (split-args t) (documentation nil) (short nil))
  (let ((newcmd (make-instance 'cmddef :name name
				       :short short
				       :fun fun
				       :split-args split-args
				       :documentation documentation)))
    (setf (gethash name *command-table*) newcmd)))

(defun parse-and-run-command (string)
  (let ((cmddef (find-cmddef string)))
    (if (not (null cmddef))
	(handler-case
	    (run-editor-command cmddef string)
	  (error (c) `(:error :command-error ,c)))
	'(:error :command-not-found))))

(defun cmd-run-lisp-form (str &rest args)
  (declare (ignore args))
  (if (typep str 'string)
      (handler-case
	  (let* ((form (read-from-string str)))
	    (eval form))
	(error (c) `(:error :invalid-form ,c)))
      `(:error :invalid-arguments)))

(define-editor-command "!" #'cmd-run-lisp-form :split-args nil :short t :documentation "Evaluate lisp forms")

(defun cmd-insert (c)
  (if (null *command-buffer*)
      (setf *command-buffer* (coerce (list c) 'string))
      (setf *command-buffer* (concatenate 'string *command-buffer* (list c)))))

(defun cmd-backspace ()
  (unless (null *command-buffer*)
    (if (equal (length *command-buffer*) 1)
	(setf *command-buffer* nil)
	(setf *command-buffer* (subseq *command-buffer* 0 (1- (length *command-buffer*)))))))

(defun cmd-reset ()
  (setf *command-buffer* nil))

(defun cmd-evaluate ()
  (let* ((cmdbuf *command-buffer*)
	 (cmd (find-cmddef *command-buffer*)))
    (prog1
	(if (null cmd)
	    '(:error :invalid-command)
	    (run-editor-command cmd cmdbuf))
      (setf *command-buffer* nil))))
