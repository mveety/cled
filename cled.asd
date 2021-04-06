;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cl-user)

(asdf:defsystem cled
  :author "Matthew Veety <mveety@gmail.com>"
  :description "Common Lisp text editor"
  :license "BSD"
  :version "0"
  :depends-on (:alexandria :split-sequence) ;;:croatoan)
  :serial t
  :components ((:file "dlist")
			   (:file "package")
			   (:file "message")
			   (:file "cmdproc")
			   (:file "textbuf")
			   (:file "textbuf-tools")
			   ))
