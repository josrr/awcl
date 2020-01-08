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
   (packed-size
    :accessor mem-entry-packed-size
    :binary-type binary-types:u16)
   (unk-10
    :accessor mem-entry-unk-10
    :binary-type binary-types:u16)
   (size
    :accessor mem-entry-size
    :binary-type binary-types:u16)))

(defconstant +mem-entry-state-end-of-list+ #xFF)
(defconstant +mem-entry-state-not-needed+ 0)
(defconstant +mem-entry-state-loaded+ 1)
(defconstant +mem-entry-state-load-me+ 2)

(defconstant +rt-sound+ 0)
(defconstant +rt-music+ 1)
(defconstant +rt-poly-anim+ 2)
(defconstant +rt-palette+ 3)
(defconstant +rt-bytecode+ 4)
(defconstant +rt-poly-cinematic+ 5)

(defparameter *resource-types* `((,+rt-sound+ . :sound)
                                 (,+rt-music+ . :music)
                                 (,+rt-poly-anim+ . :poly-anim)
                                 (,+rt-palette+ . :palette)
                                 (,+rt-bytecode+ . :bytecode)
                                 (,+rt-poly-cinematic+ . :polygon-cinematic)))

(defmethod print-object ((object mem-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "BANK~2,'0X (:BANK-OFFSET ~8X) (:PACKETP ~3@S) (:STATE ~3D) (:RANK ~3D) (:TYPE ~2,'0X)"
            (mem-entry-bank-id object)
            (mem-entry-bank-offset object)
            (mem-entry-packetp object)
            (mem-entry-state object)
            (mem-entry-rank-num object)
            (mem-entry-res-type object))))

(defun mem-entry-packetp (entry)
  (/= (mem-entry-size entry) (mem-entry-packed-size entry)))

(defun memlist-create (path)
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

(defun mem-entry-unpack (entry-bytes)
  entry-bytes)

(defparameter *bank-file-pattern* "BANK")
(defparameter *bank-file-path* #P"./data/aw01/")

(defun mem-entry-load (entry &key (pattern *bank-file-pattern*)
                               (path *bank-file-path*))
  (labels ((bank-file (bank-id)
             (let ((f1 (merge-pathnames path (format nil "~A~2,'0X" pattern bank-id)))
                   (f2 (merge-pathnames path (format nil "~A~2,'0x" pattern bank-id))))
               (if (probe-file f1)
                   f1
                   (when (probe-file f2) f2))))
           (read-entry (bank-id bank-offset size)
             (let* ((bank-file (bank-file bank-id))
                    (buffer (and bank-file
                                 (make-array size :element-type '(unsigned-byte 8)
                                                  :initial-element 0))))
               (when buffer
                 (handler-case
                     (binary-types:with-binary-file
                         (si (merge-pathnames path (format nil "~A~2,'0X" pattern bank-id)))
                       (file-position si bank-offset)
                       (read-sequence buffer si)
                       buffer)
                   (file-error (e)
                     (format *debug-io* "~A~%" e)
                     nil)
                   (error (e)
                     (format *debug-io* "~A~%" e)
                     nil))))))
    (with-slots (bank-id bank-offset size packed-size) entry
      (if (mem-entry-packetp entry)
          (mem-entry-unpack (read-entry bank-id bank-offset packed-size))
          (read-entry bank-id bank-offset entry)))))

(defun mem-entry-invalidate (entry)
  (setf (mem-entry-state entry) +mem-entry-state-not-needed+))

(defun memlist-invalidate-all (memlist)
  (map nil #'mem-entry-invalidate memlist))

(defun memlist-load (memlist)
  (loop for entry across (sort (remove-if #'(lambda (e)
                                              (/= (mem-entry-state e)
                                                  +mem-entry-state-load-me+))
                                          memlist)
                               #'> :key #'mem-entry-rank-num)
        for restype = (mem-entry-res-type entry)
        do (format *debug-io* "~S~%" entry)
        if (< restype 6)
          append (list (cdr (assoc restype *resource-types* :test #'=))
                       (list :entry entry
                             :data (mem-entry-load entry)))))
;;;;
