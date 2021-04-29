;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled)

(defvar *padwin* nil)

(defun getch ()
  (unless *padwin*
    (setf *padwin* (charms/ll:newpad 1 1))
    (charms/ll:wtimeout *padwin* -1))
  (charms/ll:wgetch *padwin*))

(defun get-key ()
  (let ((keycode (getch)))
    (if (< keycode 0)
	nil
	(if (< keycode 256)
	    (code-char keycode)
	    nil))))

(defun csi ()
  (kbd "escape")) ;; return escape for now

(defun handle-escape ()
  (case (get-key)
    (#\[ (case (get-key)
	   (#\A (kbd "up"))
	   (#\B (kbd "down"))
	   (#\C (kbd "right"))
	   (#\D (kbd "left"))
	   (#\5 (when (eq (get-key) #\~) (kbd "pageup")))
	   (#\6 (when (eq (get-key) #\~) (kbd "pagedown")))
	   (#\F (kbd "end"))
	   (#\H (kbd "home"))
	   (#\1 (csi))
	   (t (kbd "escape"))))
    (t (kbd "escape"))))

;;; I want keys from 32 (#\Space) to 126 (#\~) passed through
;;; Keys from 160 to 255 can also be passed through.
;;; both #\Rubout and #\Backspace will be mapped to (kbd "backspace")

(defun get-canonical-key ()
  (let ((k (get-key)))
    (cond
      ((null k) nil)
      ((eq (equal k #\Rubout) (eq k #\Backspace)) (kbd "rubout")) ;; rubout is always backspace
      ((eq k #\Esc) (handle-escape))
      ((eq k #\Newline) (kbd "enter"))
      ((ansi-direct-map-p (char-code k)) (char-code k)) ;; currently only handle basic ansi chars for input
      (t nil)))) ;; handle misc crap
