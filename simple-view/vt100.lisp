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
	  (#\5 (if (equal (get-key) #\~) (kbd "pageup") nil))
	  (#\6 (if (equal (get-key) #\~) (kbd "pagedown") nil))
	  (t nil))
	(case char
	  (#\A (kbd "up"))
	  (#\B (kbd "down"))
	  (#\C (kbd "right"))
	  (#\D (kbd "left"))
	  (t (kbd "escape"))))))

(defun get-canonical-key ()
  (let ((key (get-key)))
    (case key
      (#\Esc (handle-escape))
      (#\Rubout (kbd "backspace"))
      (#\Newline (kbd "enter"))
      (#\Can (kbd "C-x"))
      (#\Etx (kbd "C-c"))
      (#\Syn (kbd "C-v"))
      (#\Soh (kbd "C-a"))
      (#\Dc3 (kbd "C-s"))
      (otherwise (kbd (when (ansi-char-p key)
			(get-name-by-code (char-code key)))
		      ))
       )))
