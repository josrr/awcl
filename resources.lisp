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
  (let ((binary-types:*endian* :big-endian))
    (handler-case
        (binary-types:with-binary-file (s path)
          (let* ((file-size (file-length s))
                 (num-entries (floor file-size 20))
                 (memlist (make-array num-entries :element-type '(or null mem-entry)
                                                  :initial-element nil)))
            (handler-case
                (loop for i from 0 below num-entries
                      do (setf (aref memlist i)
                               (binary-types:read-binary 'mem-entry s))
                      finally (return memlist))
              (end-of-file (e)
                (format *debug-io* "~A~%" e)
                memlist))))
      (file-error (e)
        (format *debug-io* "~A~%" e)
        nil))))

(defstruct unpack-ctx
  (size 0 :type (unsigned-byte 16))
  (crc 0 :type (unsigned-byte 32))
  (chk 0 :type (unsigned-byte 32))
  (data-size 0 :type (unsigned-byte 32)))

(define-condition unpack-error (error)
  ((position :initarg :position :reader unpack-error-position)
   (bank-id :initarg :bank-id :reader unpack-error-bank-id))
  (:report (lambda (condition stream)
             (format stream "Error unpacking file ~S and position ~D."
                     (unpack-error-bank-id condition) (unpack-error-position condition)))))

(defun mem-entry-unpack (entry-bytes packed-size)
  (declare (optimize (speed 3))
           (type fixnum packed-size))
  (flexi-streams:with-input-from-sequence (s entry-bytes)
    (file-position s (- packed-size 12))
    (let* ((binary-types:*endian* :big-endian)
           (ctx (make-unpack-ctx :chk (binary-types:read-binary 'binary-types:u32 s)
                                 :crc (binary-types:read-binary 'binary-types:u32 s)
                                 :data-size (binary-types:read-binary 'binary-types:u32 s)))
           (obuf (make-array (unpack-ctx-data-size ctx) :element-type '(unsigned-byte 8)
                                                        :initial-element 0))
           (oi (1- (unpack-ctx-data-size ctx))))
      (declare (type fixnum oi))
      (setf (unpack-ctx-crc ctx) (logxor (unpack-ctx-crc ctx) (unpack-ctx-chk ctx)))
      (file-position s (- packed-size 16))
      (format *debug-io* "data-size:~12D  crc:~12X  chk:~12X~%obuf: ~S;  packed-size: ~10D~%file-position: ~D~%~%"
              (unpack-ctx-data-size ctx) (unpack-ctx-crc ctx) (unpack-ctx-chk ctx)
              (type-of obuf) packed-size (file-position s))
      (labels ((rcr (cf)
                 (let ((rcf (> (logand 1 (unpack-ctx-chk ctx)) 0)))
                   (setf (unpack-ctx-chk ctx) (ash (unpack-ctx-chk ctx) -1))
                   (when cf
                     (setf (unpack-ctx-chk ctx) (logior (unpack-ctx-chk ctx) #x80000000)))
                   rcf))
               (next-chunk ()
                 (let ((cf (rcr nil)))
                   (when (zerop (unpack-ctx-chk ctx))
                     (assert (>= (file-position s) 0))
                     (setf (unpack-ctx-chk ctx) (binary-types:read-binary 'binary-types:u32 s)
                           (unpack-ctx-crc ctx) (logxor (unpack-ctx-crc ctx) (unpack-ctx-chk ctx))
                           cf (rcr t))
                     (when (>= (file-position s) 8)
                         (file-position s (- (file-position s) 8))))
                   cf))
               (get-code (n)
                 (declare (type fixnum n))
                 (loop repeat n
                       for c fixnum = 0 then (ash c 1)
                       if (next-chunk) do (setf c (logior c 1))
                       finally (return c)))
               (dec-unk-1 (n add-count)
                 (loop with count = (+ 1 add-count (get-code n))
                       initially (decf (unpack-ctx-data-size ctx) count)
                       repeat count
                       do (assert (>= oi 0))
                          (setf (aref obuf oi) (get-code 8))
                          (decf oi)))
               (dec-unk-2 (n)
                 (loop with count = (1+ (unpack-ctx-size ctx))
                       and i = (get-code n)
                       initially (decf (unpack-ctx-data-size ctx) count)
                       repeat count
                       do (assert (>= oi 0))
                          (setf (aref obuf oi) (aref obuf (+ oi i)))
                          (decf oi))))
        (loop do (cond ((not (next-chunk))
                        (setf (unpack-ctx-size ctx) 1)
                        (if (not (next-chunk))
                            (dec-unk-1 3 0)
                            (dec-unk-2 8)))
                       (t (let ((c (get-code 2)))
                            (cond
                              ((= c 3) (dec-unk-1 8 8))
                              ((< c 2)
                               (setf (unpack-ctx-size ctx) (+ c 2))
                               (dec-unk-2 (+ c 9)))
                              (t (setf (unpack-ctx-size ctx) (get-code 8))
                                 (dec-unk-2 12))))))
              while (> (unpack-ctx-data-size ctx) 0))
        (values obuf (unless (zerop (unpack-ctx-crc ctx)) (file-position s)))))))

(defparameter *bank-file-pattern* "BANK")
(defparameter *bank-file-path* #P"data/aw01/")

(defun mem-entry-load (entry &key (pattern *bank-file-pattern*)
                               (path *bank-file-path*))
  (labels ((bank-file (bank-id)
             (let ((f1 (merge-pathnames path (format nil "~A~2,'0X" pattern bank-id)))
                   (f2 (merge-pathnames path (format nil "~A~(~2,'0X~)" pattern bank-id))))
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
                     (with-open-file (si bank-file :element-type '(unsigned-byte 8))
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
          (let ((bytes (read-entry bank-id bank-offset packed-size)))
            (when bytes
              (multiple-value-bind (unpacked position-crc-error)
                  (mem-entry-unpack bytes packed-size)
                (when position-crc-error
                  (error (make-condition 'unpack-error
                                         :bank-id bank-id
                                         :position position-crc-error)))
                unpacked)))
          (read-entry bank-id bank-offset size)))))

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

(defun be-ui32-a (v)
  (let ((uv 0))
    (dotimes (i (binary-types:sizeof 'binary-types:u32))
      (setf uv (+ (* uv #x100)
                  (aref v i))))
    uv))

(defun be-ui32 (w x y z)
  (logior (ash w 24) (ash x 16) (ash y 8) z))
