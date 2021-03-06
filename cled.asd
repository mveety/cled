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
  :depends-on (:alexandria :split-sequence :chanl)
  :serial t
  :components ((:file "dlist")
	       (:module "core"
		:components ((:file "package")
			     (:file "config")
			     (:file "proc")
			     (:file "message")
			     (:file "reaper")
			     (:file "cmdproc")
			     (:file "textbuf")
			     (:file "buffer")
			     (:file "buffer-manager")
			     (:file "simple-buffer")
			     (:file "window")
			     (:file "kbdutil")
			     (:file "kbdevent")
			     (:file "keyboard")
			     (:file "basic-buffer")
			     (:file "bb-search")
			     (:file "file-buffer")
			     ))
	       ))

(asdf:defsystem cled/simple-view
  :author "Matthew Veety <mveety@gmail.com>"
  :description "Common Lisp version of more?"
  :license "BSD"
  :version "0"
  :depends-on (:alexandria :cl-charms :cled)
  :serial t
  :components ((:module "simple-view"
		:components ((:file "package")
			     (:file "vt100")
			     (:file "simple-view")
			     ))
	       ))

(asdf:defsystem cled/term-view
  :author "Matthew Veety <mveety@gmail.com>"
  :description "Common Lisp version of more?"
  :license "BSD"
  :version "0"
  :depends-on (:alexandria :cl-charms :cled :split-sequence)
  :serial t
  :components ((:module "term-view"
		:components ((:file "package")
			     (:file "config")
			     (:file "keyboard")
			     (:file "input-proc")
			     (:file "edcommand")
			     (:file "minibuffer")
			     (:file "term-view")
			     ))
	       ))
