;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defun key-modifier (key-name)
  (if (and (find #\- key-name)
		   (not (string= key-name "-")))
	  (subseq key-name 0 1)
	  nil))
