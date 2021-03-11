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

(defmethod print-object ((object mem-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "BANK~2,'0X (:TYPE ~2,'0X) (:SIZE ~5D) (:PACKED-SIZE ~6D) (:PACKETP ~3@S) (:BANK-OFFSET ~8X) (:STATE ~3D) (:RANK ~3D)"
            (mem-entry-bank-id object)
            (mem-entry-res-type object)
            (mem-entry-size object)
            (mem-entry-packed-size object)
            (mem-entry-packetp object)
            (mem-entry-bank-offset object)
            (mem-entry-state object)
            (mem-entry-rank-num object))))

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
  (flexi-streams:with-input-from-sequence (s entry-bytes)
    (file-position s (- packed-size 12))
    (let* ((binary-types:*endian* :big-endian)
           (ctx (make-unpack-ctx :chk (binary-types:read-binary 'binary-types:u32 s)
                                 :crc (binary-types:read-binary 'binary-types:u32 s)
                                 :data-size (binary-types:read-binary 'binary-types:u32 s)))
           (obuf (make-array (unpack-ctx-data-size ctx) :element-type '(unsigned-byte 8)
                                                        :initial-element 0))
           (oi (1- (unpack-ctx-data-size ctx))))
      (setf (unpack-ctx-crc ctx) (logxor (unpack-ctx-crc ctx) (unpack-ctx-chk ctx)))
      (file-position s (- packed-size 16))
      (format *debug-io* "data-size:~12D  crc:~12X  chk:~12X~%obuf: ~S;  packed-size: ~10D~%file-position: ~D~%~%"
              (unpack-ctx-data-size ctx) (unpack-ctx-crc ctx) (unpack-ctx-chk ctx)
              (type-of obuf) packed-size (file-position s))
      (labels ((rcr (cf)
                 (prog1 (= (ldb (byte 1 0) (unpack-ctx-chk ctx)) 1)
                   (setf (unpack-ctx-chk ctx) (ash (unpack-ctx-chk ctx) -1))
                   (when cf
                     (setf (unpack-ctx-chk ctx) (logior (unpack-ctx-chk ctx) #x80000000)))))
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
                 (loop repeat n
                       for c = 0 then (ash c 1)
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
               (or (and (probe-file f1) f1)
                   (and (probe-file f2) f2))))
           (read-entry (bank-id bank-offset size)
             (a:when-let ((bank-file (bank-file bank-id))
                          (buffer (make-array size :element-type '(unsigned-byte 8)
                                                   :initial-element 0)))
               (handler-case
                   (with-open-file (si bank-file :element-type '(unsigned-byte 8))
                     (file-position si bank-offset)
                     (read-sequence buffer si)
                     (setf (mem-entry-state entry)
                           (if (= (mem-entry-res-type entry) +rt-poly-anim+)
                               +mem-entry-state-not-needed+
                               +mem-entry-state-loaded+))
                     buffer)
                 (file-error (e)
                   (format *debug-io* "~A~%" e)
                   nil)
                 (error (e)
                   (format *debug-io* "~A~%" e)
                   nil)))))
    (with-slots (bank-id bank-offset size packed-size) entry
      (if (mem-entry-packetp entry)
          (a:when-let ((bytes (read-entry bank-id bank-offset packed-size)))
            (multiple-value-bind (unpacked position-crc-error)
                (mem-entry-unpack bytes packed-size)
              (when position-crc-error
                (error (make-condition 'unpack-error
                                       :bank-id bank-id
                                       :position position-crc-error)))
              unpacked))
          (read-entry bank-id bank-offset size)))))

(defun mem-entry-invalidate (entry)
  (setf (mem-entry-state entry) +mem-entry-state-not-needed+))

(defun memlist-invalidate-all (memlist)
  (map nil #'mem-entry-invalidate memlist))

(defparameter *resource-types* `((,+rt-sound+ . sound)
                                 (,+rt-music+ . music)
                                 (,+rt-poly-anim+ . polygon-anim)
                                 (,+rt-palette+ . palette)
                                 (,+rt-bytecode+ . bytecode)
                                 (,+rt-poly-cinematic+ . polygon-cinematic)))

(defclass resource ()
  ((entry :reader resource-entry
          :initarg :entry)
   (data :reader resource-data
         :initarg :data)))

(defclass sound (resource) ())
(defclass music (resource) ())
(defclass polygon-anim (resource) ())
(defclass palette (resource) ())
(defclass bytecode (resource) ())
(defclass polygon-cinematic (resource) ())

(defun make-resource (restype entry data)
  (make-instance restype :entry entry :data data))

(defclass resource-manager ()
  ((memlist :reader rm-memlist
            :initarg :memlist)
   (current-part-id :accessor rm-current-part-id
                    :initform 0)
   (next-part-id :accessor rm-next-part-id
                 :initform 0)
   (script-stream :accessor rm-script-stream
                  :initform nil)
   (video-stream :accessor rm-video-stream
                 :initform nil)
   (palette-stream :accessor rm-palette-stream
                   :initform nil)
   (cinematic-stream :accessor rm-cinematic-stream
                     :initform nil)
   ;;(seg-palettes :accessor rm-seg-palettes :initform nil)
   ;;(seg-bytecode :accessor rm-seg-bytecode :initform nil)
   ;;(seg-cinematic :accessor rm-seg-cinematic :initform nil)
   (seg-video2 :accessor rm-seg-video2
               :initform nil)
   (use-seg-video2 :accessor rm-use-seg-video2
                   :initform nil)))

(defun rm-load-resources (rm &optional (first-time t))
  (loop initially (when first-time
                    (setf (rm-video-stream rm) nil
                          (rm-script-stream rm) nil
                          (rm-palette-stream rm) nil
                          (rm-cinematic-stream rm) nil))
        for entry across (sort (remove-if #'(lambda (e)
                                              (/= (mem-entry-state e)
                                                  +mem-entry-state-load-me+))
                                          (rm-memlist rm))
                               #'> :key #'mem-entry-rank-num)
        for restype = (cdr (assoc (mem-entry-res-type entry) *resource-types*))
        for res = (and restype
                       (make-resource restype entry (mem-entry-load entry)))
        do (format *debug-io* "res: ~S~%" res)
        when res
          do (case restype
               ((polygon-anim) (setf (rm-video-stream rm)
                                     (flexi-streams:make-in-memory-input-stream
                                      (resource-data res))))
               ((polygon-cinematic) (setf (rm-cinematic-stream rm)
                                          (flexi-streams:make-in-memory-input-stream
                                           (resource-data res))))
               ((bytecode) (setf (rm-script-stream rm)
                                 (flexi-streams:make-in-memory-input-stream
                                  (resource-data res))))
               ((palette) (setf (rm-palette-stream rm)
                                (flexi-streams:make-in-memory-input-stream
                                 (resource-data res)))))))

(defun rm-setup-part (rm part-id &optional (memlist-parts *memlist-parts*))
  (assert (or (>= part-id +game-part-first+)
              (<= part-id +game-part-last+)))
  (when (/= (rm-current-part-id rm) part-id)
    (let ((memlist (rm-memlist rm)))
      (a:when-let* ((idx (- part-id +game-part-first+))
                    (part-desc (aref memlist-parts idx))
                    (palette-entry (aref memlist (getf part-desc :palette)))
                    (bytecode-entry (aref memlist (getf part-desc :code)))
                    (video-1-entry (aref memlist (getf part-desc :video-1)))
                    (video-2-idx (getf part-desc :video-2)))
        (memlist-invalidate-all memlist)
        (setf (mem-entry-state palette-entry) +mem-entry-state-load-me+
              (mem-entry-state bytecode-entry) +mem-entry-state-load-me+
              (mem-entry-state video-1-entry) +mem-entry-state-load-me+)
        (when (/= video-2-idx +memlist-part-none+)
          (setf (mem-entry-state (aref memlist video-2-idx)) +mem-entry-state-load-me+
                (rm-seg-video2 rm) (mem-entry-buffer (aref memlist video-2-idx))))
        (rm-load-resources rm)
        (setf ;;(rm-seg-palettes rm) (mem-entry-buffer palette-entry)
              ;;(rm-seg-bytecode rm) (mem-entry-buffer bytecode-entry)
              ;;(rm-seg-cinematic rm) (mem-entry-buffer video-1-entry)
              (rm-current-part-id rm) part-id)))))

(defun rm-load-part (rm part-id)
  (assert (or (>= part-id +game-part-first+)
              (<= part-id +game-part-last+)))
  (setf (rm-next-part-id rm) part-id)
  t)

(defun rm-load-memory-entry (rm entry-id)
  (let ((entry (aref (rm-memlist rm) entry-id)))
    (when (= (mem-entry-state entry) +mem-entry-state-not-needed+)
      (setf (mem-entry-state entry) +mem-entry-state-load-me+)
      (rm-load-resources rm nil))))

(defun rm-setup-next-part (rm)
  (with-slots (next-part-id) rm
    (when (/= 0 next-part-id)
      (rm-setup-part rm next-part-id)
      (setf next-part-id 0))))

;;;;
(defun be-ui32-a (v)
  (let ((uv 0))
    (dotimes (i (binary-types:sizeof 'binary-types:u32))
      (setf uv (+ (* uv #x100)
                  (aref v i))))
    uv))

(defun be-ui32 (w x y z)
  (logior (ash w 24) (ash x 16) (ash y 8) z))
