;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(defpackage :cled-core
  (:use :cl :dlist :alexandria :split-sequence)
  (:export
   ;; message passing bits
   #:port
   #:msg
   #:sendmsg
   #:getreply
   #:message
   #:reply
   #:waitformsg
   ))
