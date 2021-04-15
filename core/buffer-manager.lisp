;;; -*- Syntax: Common-Lisp -*-
;;; cled -- Text editor in common lisp
;;; Copyright 2021 Matthew Veety. Under BSD License
;;; See LICENSE for details.

(in-package :cled-core)

(defparameter *managed-buffers* (make-hash-table :test 'equal))

(defun buffer-name-key-exists (buf)
  (if (member (buffer-name buf)
	      (hash-table-keys *managed-buffers*))
      t
      nil))

(defun buffer-id-key-exists (buf)
  (if (member (buffer-id buf)
	      (hash-table-keys *managed-buffers*))
      t
      nil))

(defun quick-get-buffer (name)
  (gethash name *managed-buffers*))

(defun manage-buffer (buf)
  (let ((buffer-name (buffer-name buf))
	(buffer-id (buffer-id buf)))
    (setf (gethash
	   (if (buffer-name-key-exists buf)
	       buffer-id
	       buffer-name)
	   *managed-buffers*)
	  buf)))

(defun unmanage-buffer (buf &key (kill t))
  (let* ((buffer-name (buffer-name buf))
	 (buffer-id (buffer-id buf))
	 (found-buf (quick-get-buffer buffer-name)))
    (when (or (buffer-name-key-exists buf)
	      (buffer-id-key-exists buf))
      (if (string= (buffer-id buf) (buffer-id found-buf))
	  (remhash buffer-name *managed-buffers*)
	  (progn
	    (setf found-buf (gethash buffer-id *managed-buffers*))
	    (remhash buffer-id *managed-buffers*)))
      (when kill
	(sendcmd found-buf :end-command)))))

(defun all-managed-buffers ()
  (hash-table-keys *managed-buffers*))
