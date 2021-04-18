;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage curses-keys
  (:use :cl)
  (:export #:curses-keys))

(in-package :curses-keys)

(defun curses-keys (&optional (ostream *standard-output*))
  (let ((key nil)
	)
    (charms:with-curses ()
      (charms:disable-echoing)
      (charms:clear-window charms:*standard-window*)
      (charms:enable-raw-input :interpret-control-characters nil)
      ;;;(charms:enable-non-blocking-mode charms:*standard-window*)
      (loop named main-loop
	    do (progn
		 (setf key (charms:get-char charms:*standard-window* :ignore-error t))
		 (unless (equal key nil)
		   (format ostream "[key press: ~S] " key)))))))
