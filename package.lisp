;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(defpackage :cled-core
  (:use :cl :dlist :alexandria :split-sequence)
  (:export
   ;; message passing bits
   #:port #:msg #:sendmsg #:getreply
   #:message #:reply #:waitformsg
   ;; message passing event loop
   #:add-command #:command-exists #:find-command #:del-command
   #:run-command #:run-table #:sendcmd #:get-command-list
   #:defcommand #:add-template-to-cmd-table
   ;; textbuf
   #:set-dot #:get-dot #:insert-line #:remove-line
   #:insert-char #:remove-char #:get-char #:set-char
   #:get-line #:line-length
   ;; textbuf-tools
   #:make-textbuf #:tbuf-to-list #:get-n-lines #:line-list-to-cs
   #:line-list-to-strings #:line-list-to-chars #:merge-lines #:split-line
   ;; buffer
   #:*buffer-types* #:make-buffer #:define-buffer-type #:buffer
   #:buffer-type #:buffer-type-string #:buffer-name
   ))
