;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(defpackage :cled-core
  (:use :cl :dlist :alexandria :split-sequence)
  (:export
   ;; configuration
   #:*make-backup-files*
   ;; process management
   #:process #:proc-entry #:proc-exit #:start-process
   #:stop-process #:delete-process #:no-process-restart #:process-restart
   #:start-process-in-current-thread #:all-processes #:running-processes
   ;; message passing bits
   #:port #:msg #:sendmsg #:getreply
   #:message #:reply #:waitformsg #:message-in-flight-p
   #:get-in-flight-message #:checkformsg #:noreturn-sendcmd
   ;; the grim reaper
   #:*grim-reaper* #:*reaper-messages* #:start-grim-reaper #:alert-reaper
   #:stop-reaper #:*reaper-print-name
   ;; message passing event loop
   #:add-command #:command-exists #:find-command #:del-command
   #:run-command #:run-table #:sendcmd #:get-command-list
   #:defcommand #:add-template-to-cmd-table
   ;; textbuf
   #:set-dot #:get-dot #:insert-line #:remove-line
   #:insert-char #:remove-char #:get-char #:set-char
   #:get-line #:line-length #:linen #:set-linen
   #:tbuf-length
   #:make-textbuf #:tbuf-to-list #:get-n-lines #:line-list-to-cs
   #:line-list-to-strings #:line-list-to-chars #:merge-lines #:split-line
   #:append-list-to-tbuf
   ;; buffer
   #:*buffer-types* #:make-buffer #:define-buffer-type #:buffer
   #:buffer-type #:buffer-type-string #:buffer-name #:buffer-id
   ;; quick and dirty buffer management
   #:*managed-buffers* #:buffer-name-key-exists #:buffer-id-key-exists
   #:quick-get-buffer #:manage-buffer #:unmanage-buffer #:all-managed-buffers
   ;; simple-buffer
   #:simple-buffer #:*simple-buffer-cmd-template*
   #:buffer-dirty-p #:set-buffer-dirty #:unset-buffer-dirty #:get-buffer-update
   #:set-buffer-owner #:set-name #:show-buffer #:hide-buffer #:buffer-nlines
   #:cursor-up #:cursor-down #:cursor-left #:cursor-right
   #:set-cursor #:get-cursor
   #:buffer-backspace #:buffer-insert-char #:buffer-tab #:buffer-newline
   #:buffer-space #:buffer-copy-char #:buffer-cut-char #:make-simple-buffer
   ;; window
   #:scroll-up #:scroll-down #:window-update #:window-resize
   #:window-process #:make-window
   ;; keyboard utils
   #:key-modifier
   ;; keyboard events
   #:kbd-event-table #:define-kbd-event #:define-default-kbd-event #:get-kbd-event
   #:generate-kbd-event #:send-kbd-event #:make-kbd-event-table
   ;; universal key codes
   #:defkey #:get-code-by-name #:get-sym-by-name #:get-name-by-sym
   #:get-name-by-code #:kbd #:ansi-char-p #:backspace-p
   #:ansi-direct-map-p
   ;; basic-buffer
   #:basic-buffer #:*basic-buffer-cmd-template*
   #:bb-set-mark #:bb-get-mark #:bb-cut-and-copy #:bb-cut
   #:bb-copy #:bb-paste #:make-basic-buffer #:bb-cut-and-copy-line
   #:bb-search
   ;; file-buffer
   #:file-buffer #:file-read #:file-write
   ))
