;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

;;; THE UNIVERSAL KEYCODES
;;; a lot of this basic design and all of the codes are cribbed from
;;; lem. 

(in-package :cled-core)

(defparameter *keynames* (make-hash-table))
(defparameter *keycodes* (make-hash-table :test 'equal))
(defparameter *keysyms* (make-hash-table :test 'equal))

(defun defkey (name code)
  (setf (gethash name *keycodes*) code)
  (when (<= code 127)
    (setf (gethash name *keysyms*) (code-char code)))
  (setf (gethash code *keynames*) name))

(defun get-code-by-name (name)
  (let ((code (gethash name *keycodes*)))
    code))

(defun get-name-by-code (code)
  (let ((name (gethash code *keynames*)))
    name))

(defun kbd (&rest key-names)
  (let ((klist (mapcar #'get-code-by-name key-names)))
    (if (equal (length klist) 1)
	(car klist)
	klist)))

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

(defkey "rubout" 127)
(defkey "tab" 9)

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
(defkey "enter" 10)
(defkey "delete" #o512)
(defkey "C-delete" 519)
(defkey "S-up" #o520)
(defkey "S-down" #o521)
(defkey "pagedown" #o522)
(defkey "pageup" #o523)
(defkey "S-tab" #o541)
(defkey "end" #o550)
(defkey "S-delete" #o577)
(defkey "S-end" #o602)
(defkey "S-home" #o607)
(defkey "S-left" #o611)
(defkey "S-pagedown" #o614)
(defkey "S-pageup" #o616)
(defkey "S-right" #o622)

(loop for code from 0 below 13
      do (let ((scode (format nil "f~A" code)))
	   (defkey scode (+ #o410 code))
	   (when (> code 0)
	     (defkey (concatenate 'string "S-" scode) (+ #o424 code)))))

(defun ansi-char-p (char)
  (if (not (null char))
      (and (>= (char-code char) 27)
	   (< (char-code char) 127))
      nil))

(defun backspace-p (ccode)
  (or (= (kbd "backspace") ccode)
      (= (kbd "rubout") ccode)))

(defun ansi-direct-map-p (code)
  (or (>= code 0)
      (<= code 127)))
