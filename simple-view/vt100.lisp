;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled)

(defmacro get-key ()
  `(charms:get-char charms:*standard-window* :ignore-error t))

(defun handle-escape ()
  (let ((char (get-key)))
    (if (or (equal char #\[)
	    (equal char #\O))
	(case (get-key)
	  (#\A (kbd "up"))
	  (#\B (kbd "down"))
	  (#\C (kbd "right"))
	  (#\D (kbd "left"))
	  (t nil))
	(case char
	  (#\A (kbd "up"))
	  (#\B (kbd "down"))
	  (#\C (kbd "right"))
	  (#\D (kbd "left"))
	  (t nil)))))

(defun get-canonical-key ()
  (let ((key (get-key)))
    (case key
      (#\Esc (handle-escape))
      (#\Rubout (kbd "backspace"))
      (#\Newline (kbd "enter"))
      (otherwise (kbd (unless (null key) (get-name-by-code (char-code key)))))
       )))
