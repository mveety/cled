;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled)

(defvar *last-message* nil)
(defvar *print-last-message* nil)

(defun make-eql-string (len)
  (coerce (loop for x from 0 below len collect #\=) 'string))

(defun string-prune (maxsz str)
  (let ((slen (length str)))
    (if (>= slen maxsz)
	(subseq str 0 maxsz)
	str)))

(defun draw-minibuf (window nlines ncols cur-dot mode-char)
  ;; draw the status bar
  (unless (<= ncols 15)
    (let* ((dotstring (format nil " (~A, ~A) ~A" (car cur-dot) (cadr cur-dot) mode-char))
	   (eqlstr (make-eql-string (- ncols (+ (length dotstring) 2))))
	   (statusbar (concatenate 'string eqlstr dotstring)))
      (charms:write-string-at-point window statusbar 0 (- nlines 2))))
  ;; draw command buffer
  (unless (null *command-buffer*)
    (let ((cbuf-len (length *command-buffer*)))
      (charms:write-string-at-point window
				    (if *print-last-message*
					(string-prune ncols *last-message*)
					(if (>= cbuf-len ncols)
					    (subseq *command-buffer* (- cbuf-len (1- ncols)) cbuf-len)
					    *command-buffer*))
				    0 (1- nlines)))))

(defun minibuf-dot (nlines ncols)
  `(,(1- nlines)
    ,(if (null *command-buffer*) 0
	 (if (>= (length *command-buffer*) ncols)
	     (1- ncols)
	     (length *command-buffer*)))))

(defun run-in-minibuf ()
  (setf *print-last-message* t)
  (setf *last-message* (format nil "~S" (cmd-evaluate))))

(defun valid-minibuf-printable-p (c)
  (and (>= c (kbd "space"))
       (< c (kbd "rubout"))))

(defun minibuf-mode-keys (c mode win)
  (declare (ignore mode win))
  (setf *print-last-message* nil)
  (if (valid-minibuf-printable-p c)
      (progn
	(cmd-insert (code-char c))
	nil)
      (case c
	(#.(kbd "escape") (cmd-reset) 'end-mode)
	(#.(kbd "rubout") (cmd-backspace) nil)
	(#.(kbd "enter") (run-in-minibuf) 'end-mode)
	(otherwise nil))))
