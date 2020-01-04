;;;; awcl.lisp

(in-package #:awcl)

(binary-types:define-binary-class mem-entry ()
  ((state
    :accessor mem-entry-state
    :binary-type binary-types:u8)
   (res-type
    :accessor mem-entry-res-type
    :binary-type binary-types:u8)
   (buffer
    :accessor mem-entry-buffer
    :binary-type binary-types:u16)
   (unk-4
    :accessor mem-entry-unk-4
    :binary-type binary-types:u16)
   (rank-num
    :accessor mem-entry-rank-num
    :binary-type binary-types:u8)
   (bank-id
    :accessor mem-entry-bank-id
    :binary-type binary-types:u8)
   (bank-offset
    :accessor mem-entry-bank-offset
    :binary-type binary-types:u32)
   (unk-c
    :accessor mem-entry-unk-c
    :binary-type binary-types:u16)
   (packet-size
    :accessor mem-entry-packet-size
    :binary-type binary-types:u16)
   (unk-10
    :accessor mem-entry-unk-10
    :binary-type binary-types:u16)
   (size
    :accessor mem-entry-size
    :binary-type binary-types:u16)))

(defun mem-entry-packetp (entry)
  (/= (mem-entry-size entry) (mem-entry-packet-size entry)))

(defun read-memlist (path)
  ;;(declare (optimize (speed 3)))
  (let ((binary-types:*endian* :big-endian))
    (handler-case
        (binary-types:with-binary-file (s path)
          (let* ((file-size (file-length s))
                 (num-entries (floor file-size 20))
                 (memlist (make-array num-entries :element-type '(or null mem-entry)
                                                  :initial-element nil)))
            (declare (type fixnum file-size num-entries))
            (handler-case
                (loop for i from 0 below num-entries
                      for entry = (binary-types:read-binary 'mem-entry s)
                      do (setf (aref memlist i) entry)
                      finally (return memlist))
              (end-of-file (e)
                (format *debug-io* "~A~%" e)
                memlist))))
      (file-error (e)
        (format *debug-io* "~A~%" e)
        nil))))

(defmethod print-object ((object mem-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "(:BANK-ID ~3D) (:BANK-OFFSET ~10D) (:PACKETP ~3@S)"
            (mem-entry-bank-id object)
            (mem-entry-bank-offset object)
            (mem-entry-packetp object))))
