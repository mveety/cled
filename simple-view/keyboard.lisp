;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

;;; a lot of this basic design and all of the codes are cribbed from
;;; lem. 

(in-package :cled)

(defparameter *keynames* (make-hash-table))
(defparameter *keycodes* (make-hash-table :test 'equal))

(defun defkey (name code)
  (setf (gethash name *keycodes*) code)
  (setf (gethash code *keynames*) name))

(defun get-code-by-name (name)
  (let ((code (gethash name *keycodes*)))
    code))

(defun get-name-by-code (code)
  (let ((name (gethash code *keynames*)))
    name))

(defun kbd (&rest key-names)
  (mapcar #'get-code-by-name key-names))

(defkey "C-@"       0)
(loop for code from 1 below 27
      do (let ((schar (string (code-char (+ 96 code)))))
	   (defkey (concatenate 'string "C-" schar) code)))
(defkey "escape"    27)
(defkey "C-\\"      28)
(defkey "C-]"       29)
(defkey "C-^"       30)
(defkey "C-_"       31)
(defkey "space"     32)

(loop for code from 33 below 127
      do (let ((schar (string (code-char code))))
	   (defkey schar code)))

(defkey "down"      #o402)
(defkey "up"        #o403)
(defkey "left"      #o404)
(defkey "right"     #o405)
(defkey "C-down"    525)
(defkey "C-up"      566)
(defkey "C-left"    545)
(defkey "C-right"   560)
(defkey "home"      #o406)
(defkey "backspace" #o407)

(loop for code from 0 below 13
      do (let ((scode (format nil "f~A" code)))
	   (defkey scode (+ #o410 code))
	   (when (> code 0)
	     (defkey (concatenate 'string "S-" scode) (+ #o424 code)))))

(defun get-key (getch-fun)
  (flet ((getc () (funcall getch-fun)))
    (let ((c (getc)))
      (case c
	(#\Esc (if (equal (getc) #\[)
		   (case (getc)
		     (#\A (get-code-by-name "up"))
		     (#\B (get-code-by-name "down"))
		     (#\C (get-code-by-name "right"))
		     (#\D (get-code-by-name "left")))
		   nil))
	(#\Rubout (get-code-by-name "backspace"))
	(#\Newline (get-code-by-name "C-j")) ;; newline is C-j
	(t c)))))
