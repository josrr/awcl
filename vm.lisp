;;;; vm.lisp

(in-package #:awcl)

(defparameter *num-channels* 64)
(defparameter *num-variables* 256)

(defconstant +vm-no-setvec-requested+ #xFFFF)
(defconstant +vm-inactive-channel+ #xFFFF)
(defconstant +vm-variable-random-seed+ #x3C)
(defconstant +vm-variable-last-keychar+ #xDA)
(defconstant +vm-variable-hero-pos-up-down+ #xE5)
(defconstant +vm-variable-mus-mark+ #xF4)
(defconstant +vm-variable-scroll-y+ #xF9)
(defconstant +vm-variable-hero-action+ #xFA)
(defconstant +vm-variable-hero-pos-jump-down+ #xFB)
(defconstant +vm-variable-hero-pos-left-right+ #xFC)
(defconstant +vm-variable-hero-pos-mask+ #xFD)
(defconstant +vm-variable-hero-action-pos-mask+ #xFE)
(defconstant +vm-variable-pause-slices+ #xFF)

(defclass channel-state ()
  ((current :accessor channel-state-current
            :initform nil
            :initarg :current)
   (requested :accessor channel-state-requested
              :initform nil
              :initarg :requested)))

(defclass channel ()
  ((id :reader channel-id
       :initarg :id)
   (pc-offset :accessor channel-pc-offset
              :initform +vm-inactive-channel+
              :initarg :pc-offset)
   (requested-pc-offset :accessor channel-requested-pc-offset
                        :initform 0
                        :initarg :requested-pc-offset)
   (state :reader channel-state
          :initform (make-instance 'channel-state)
          :initarg :is-active)))

(defmethod print-object ((object channel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "id=0x~2,'0X pc-offset=0x~4,'0X requested-pc-offset=0x~4,'0X"
            (channel-id object)
            (channel-pc-offset object)
            (channel-requested-pc-offset object))))

(defun channel-is-active-p (channel)
  (/= (channel-pc-offset channel) +vm-inactive-channel+))

(defstruct vm
  ;;(memlist nil :type (or null (simple-array (or null mem-entry) *)))
  (fast-mode nil :type boolean)
  (resource-manager nil :type resource-manager)
  ;;(resources nil :type list)
  (num-variables *num-variables* :type fixnum)
  (num-channels *num-channels* :type fixnum)
  (variables nil :type (or null (simple-array (signed-byte 16) *)))
  (stack-calls nil :type (or null (simple-array (signed-byte 16) *)))
  (stack-pos 0 :type fixnum)
  (channels nil :type (or null (simple-array channel)))
  (last-time-stamp 0)
  (frame nil))

(defun vm-create (memlist-path frame)
  (let ((vm (make-vm :fast-mode nil
                     :resource-manager (make-instance 'resource-manager
                                                      :memlist (memlist-create memlist-path))
                     :num-variables *num-variables*
                     :num-channels *num-channels*
                     :variables (make-array *num-variables*
                                            :initial-element 0
                                            :element-type '(signed-byte 16))
                     :stack-calls (make-array *num-channels*
                                              :initial-element 0
                                              :element-type '(signed-byte 16))
                     :channels (make-array *num-channels*
                                           :initial-contents (loop for i from 0 below *num-channels*
                                                                   collect (make-instance 'channel :id i))
                                           :element-type 'channel)
                     :frame frame)))
    (vm-init vm)
    vm))

(defun check-channel-requests (vm)
  (when (rm-setup-next-part (vm-resource-manager vm))
    (loop for c across (vm-channels vm)
          do (setf (channel-pc-offset c) +vm-inactive-channel+
                   (channel-requested-pc-offset c) +vm-no-setvec-requested+))
    (setf (channel-pc-offset (aref (vm-channels vm) 0)) 0))
  (loop for channel across (vm-channels vm)
        for offset = (channel-requested-pc-offset channel)
        for state = (channel-state channel)
        do (setf (channel-state-current state) (channel-state-requested state))
        when (/= offset +vm-no-setvec-requested+)
          do (setf (channel-pc-offset channel) (if (= offset #xFFFE)
                                                   +vm-inactive-channel+ offset)
                   (channel-requested-pc-offset channel) +vm-no-setvec-requested+)))

(defparameter *vm-opcodes* (make-array #xFF :element-type '(or null function)))

(defun vm-op-function (opcode &optional (opcodes *vm-opcodes*))
  (or (aref opcodes opcode)
      (lambda (channel)
        (declare (ignore channel))
        t)))

(define-condition runtime-error (error)
  ((position :initarg :position :reader runtime-error-position)
   (opcode :initarg :opcode :reader runtime-error-opcode))
  (:report (lambda (condition stream)
             (format stream "Error runing bytecode opcode 0x~X in position ~D."
                     (runtime-error-opcode condition)
                     (runtime-error-position condition)))))

;;(declaim (inline fetch-word fetch-byte))
(declaim (inline fetch-byte fetch-word fetch-sword))
(defun fetch-word (stream)
  (let ((binary-types:*endian* :big-endian))
    (binary-types:read-binary 'binary-types:u16 stream)))

(defun fetch-sword (stream)
  (let ((binary-types:*endian* :big-endian))
    (binary-types:read-binary 'binary-types:s16 stream)))

(defun fetch-byte (stream)
  (binary-types:read-binary 'binary-types:u8 stream))

(defun vm-draw-polygon (vm offset x y)
  (format *debug-io* "draw-polygon offset=~D x=~3D y=~3D~%" offset x y)
  (awcl-draw-polygon (vm-frame vm) offset #xFF #x40 x y))

(defun run-channel (vm)
  (loop with stop = nil
        and rm = (vm-resource-manager vm)
        and frame = (vm-frame vm)
        with script-stream = (rm-script-stream rm)
        for opcode = (fetch-byte script-stream)
        if (= (ldb (byte 1 7) opcode) 1) do
          ;; #x80
          (setf (rm-use-seg-video2 rm) nil)
          (let* ((off (logand (* 2 (logior (ash opcode 8)
                                           (fetch-byte script-stream)))
                              #xFFFF))
                 (x (fetch-byte script-stream))
                 (y (fetch-byte script-stream))
                 (h (- y 199)))
            (when (plusp h)
              (setf y 199)
              (incf x h))
            (format *debug-io*
                    "[0x80] opcode=0x~4,'0X ; 0x~6,'0x ~3,'0d ~3,'0d~%"
                    opcode off x y)
            (awcl-draw-polygon frame off #xFF #x40 x y))
        else if (= (ldb (byte 1 6) opcode) 1) do
          ;; #x40
          (setf (rm-use-seg-video2 rm) nil)
          (let ((off (logand (* 2 (fetch-word script-stream))
                             #xFFFF))
                (y)
                (x (fetch-byte script-stream))
                (zoom))
            (if (zerop (logand opcode #x20))
                (setf x (if (zerop (logand opcode #x10))
                            (logior (ash x 8) (fetch-byte script-stream))
                            (aref (vm-variables vm) x)))
                (when (= (logand opcode #x10) 1)
                  (logand (incf x #x100) #xFFFF)))
            (setf y (fetch-byte script-stream))
            (when (zerop (logand opcode #x8))
              (setf y (if (zerop (logand opcode #x4))
                          (logior (ash y 8) (fetch-byte script-stream))
                          (aref (vm-variables vm) y))))
            (setf zoom (fetch-byte script-stream))
            (if (zerop (logand opcode #x2))
                (setf zoom (if (zerop (logand opcode #x1))
                               (prog1 #x40
                                 (file-position script-stream
                                                (1- (file-position script-stream))))
                               (aref (vm-variables vm) zoom)))
                (when (= (logand opcode #x1) 1)
                  (setf (rm-use-seg-video2 rm) t
                        zoom #x40)
                  (file-position script-stream
                                 (1- (file-position script-stream)))))
            (format *debug-io*
                    "[0x40] opcode=0x~4,'0X ; 0x~6,'0x ~3,'0d ~3,'0d~%"
                    opcode off x y)
            (awcl-draw-polygon (vm-frame vm) off #xFF zoom x y))
        else if (> opcode #x1a) do
          (format *debug-io* "[ >  #x1a]  opcode=~4X  ⸺  ~%" opcode)
          (error (make-condition 'runtime-error
                                 :opcode opcode
                                 :position (file-position script-stream)))
        else do
          ;;(format *debug-io* "Opcode: 0x~4,'0X " opcode)
          (setf stop (funcall (vm-op-function opcode) vm script-stream))
        end
        until stop))

(defun run-one-frame (vm)
  (loop with script-stream = (rm-script-stream (vm-resource-manager vm))
        for channel across (vm-channels vm)
        when (and (null (channel-state-current (channel-state channel)))
                  (channel-is-active-p channel))
          do (format *debug-io* "channel[0]: ~S~%" channel)
             (file-position script-stream (channel-pc-offset channel))
             (format *debug-io* "channel[1]: ~D~%" (file-position script-stream))
             (run-channel vm)
             (setf (channel-pc-offset channel) (file-position script-stream))
             (format *debug-io* "channel[2]: ~S~%" channel)))

;;;;
(defun vm-change-part (vm part-id)
  (setf (aref (vm-variables vm) #xE4) #x14)
  (rm-setup-part (vm-resource-manager vm) part-id)
  (loop for c across (vm-channels vm)
        do (setf (channel-pc-offset c) +vm-inactive-channel+
                 (channel-requested-pc-offset c) +vm-inactive-channel+
                 (channel-state-current (channel-state c)) nil
                 (channel-state-requested (channel-state c)) nil))
  (setf (channel-pc-offset (aref (vm-channels vm) 0)) 0)
  (values))

(defun vm-init (vm)
  (let ((variables (vm-variables vm)))
    (dotimes (i (vm-num-variables vm))
      (setf (aref variables i) 0))
    (setf (aref variables #x54) #x81
          (aref variables +vm-variable-random-seed+) (logand (get-universal-time) #x7fff)
          ;; (sfxplayer-mark-var (getf vm :player)) (aref variables +variable-mus-mark+)
          (aref variables #xBC) #x10
          (aref variables #xC6) #x80
          (aref variables #xF2) 4000
          (aref variables #xDC) 33
          (vm-fast-mode vm) nil)))

(defun vm-run (vm)
  (check-channel-requests vm)
  (run-one-frame vm))

;;;;
(defmacro def-op-function ((vm stream (name opcode)) &body body)
  (let ((g-func (gensym)))
    `(let ((,g-func (lambda (,vm ,stream)
                      ,@body)))
       (setf (symbol-function (alexandria:symbolicate ',name '-op)) ,g-func
             (aref *vm-opcodes* ,opcode) ,g-func))))

(def-op-function (vm stream (cmov #x00))
  (let ((var-id (fetch-byte stream))
        (value (fetch-sword stream)))
    (format *debug-io* "CMOV var-id=0x~2,'0X value=0x~X~%" var-id value)
    (setf (aref (vm-variables vm) var-id) value))
  nil)

(def-op-function (vm stream (mov #x01))
  (let ((dst-var (fetch-byte stream))
        (src-var (fetch-byte stream)))
    (format *debug-io* "MOV dst-var=0x~2,'0X src-var=0x~2,'0X~%" dst-var src-var)
    (setf (aref (vm-variables vm) dst-var) (aref (vm-variables vm) src-var)))
  nil)

(def-op-function (vm stream (add #x02))
  (let ((dst-var (fetch-byte stream))
        (src-var (fetch-byte stream)))
    (format *debug-io* "ADD dst-var=0x~2,'0X src-var=0x~2,'0X~%" dst-var src-var)
    (incf (aref (vm-variables vm) dst-var) (aref (vm-variables vm) src-var)))
  nil)

(def-op-function (vm stream (cadd #x03))
  (let ((var-id (fetch-byte stream))
        (value (fetch-sword stream)))
    (declare (type (signed-byte 16) value))
    (incf (aref (vm-variables vm) var-id) value)
    (format *debug-io* "CADD var-id=0x~2,'0X value=0x~X [~6d]~%" var-id value
            (aref (vm-variables vm) var-id)))
  nil)

(def-op-function (vm stream (call #x04))
  (let ((offset (fetch-word stream)))
    (format *debug-io* "CALL offset=0x~X~%" offset)
    (setf (aref (vm-stack-calls vm) (vm-stack-pos vm))
          (file-position stream))
    (when (= #xFF (vm-stack-pos vm))
      (error "call: stack overflow"))
    (incf (vm-stack-pos vm))
    (file-position stream offset))
  nil)

(def-op-function (vm stream (ret #x05))
  (when (zerop (vm-stack-pos vm))
    (error "call: stack underflow"))
  (decf (vm-stack-pos vm))
  (let ((stack (vm-stack-calls vm)))
    (format *debug-io* "RET offset=0x~X~%" (aref stack (vm-stack-pos vm)))
    (file-position stream (aref stack (vm-stack-pos vm))))
  nil)

(def-op-function (vm stream (pause-channel #x06))
  (declare (ignore vm stream))
  (format *debug-io* "PAUSE-CHANNEL~%")
  t)

(def-op-function (vm stream (jmp #x07))
  (declare (ignore vm))
  (let ((offset (fetch-word stream)))
    (format *debug-io* "JMP offset=0x~X~%" offset)
    (file-position stream offset))
  nil)

(def-op-function (vm stream (set-vect #x08))
  (let ((channel-id (fetch-byte stream))
        (offset (fetch-word stream)))
    (format *debug-io* "SET-VECT channel-id=~D  offset=0x~X~%"
            channel-id offset)
    (setf (channel-requested-pc-offset (aref (vm-channels vm) channel-id))
          offset))
  nil)

(def-op-function (vm stream (jnz #x09))
  (let ((var-id (fetch-byte stream)))
    (format *debug-io* "JNZ var-id=~D~%" var-id)
    (decf (aref (vm-variables vm) var-id))
    (if (not (zerop (aref (vm-variables vm) var-id)))
        (jmp-op vm stream)
        (fetch-word stream)))
  nil)

(def-op-function (vm stream (cond-jmp #x0A))
  (let* ((opcode (fetch-byte stream))
         (condition (logand opcode #x7))
         (b-id (fetch-byte stream))
         (b (aref (vm-variables vm) b-id))
         (a (cond
              ((= (ldb (byte 1 7) opcode) 1) ; #x80
               (aref (vm-variables vm) (fetch-byte stream)))
              ((= (ldb (byte 1 6) opcode) 1) ; #x40
               (fetch-sword stream))
              (t (fetch-byte stream))))
         (func (if (<= 0 condition 5)
                   (aref '#(= /= > >= < <=) condition)
                   (error "cond-jmp-op: invalid condition ~d" condition))))
    (format *debug-io*
            "COND-JMP opcode=0x~X condition=0x~X func=~S b[0x~X]=0x~X a=0x~X~%"
            opcode condition func b-id b a)
    (if (funcall func b a)
        (jmp-op vm stream)
        (fetch-word stream)))
  nil)

(def-op-function (vm stream (set-pal #x0B))
  (let ((palette-id (ash (fetch-word stream) -8))
        (frame (vm-frame vm)))
    (format *debug-io* "SET-PAL palette-id=0x~X~%" palette-id)
    ;;(break)
    (setf (awcl-palette-id-requested frame) palette-id)
    ;;(awcl-change-palette frame)
    )
  nil)

(def-op-function (vm stream (reset-channel #x0C))
  (let ((channel-id (fetch-byte stream))
        (i (a:clamp (fetch-byte stream) 0 (1- *num-channels*))))
    (format *debug-io* "RESET-CHANNEL channel-id=~D i=~D~%" channel-id i)
    (if (> channel-id i)
        (warn "reset-channel: ec=0x~X (i > channel-id)" #x880)
        (let ((a (fetch-byte stream))
              (channels (vm-channels vm)))
          (cond
            ((= 2 a)
             (loop for j from channel-id to i
                   do (setf (channel-requested-pc-offset (aref channels j))
                            #xFFFE)))
            ((< a 2)
             (loop for j from channel-id to i
                   do (setf (channel-state-requested (channel-state (aref channels j)))
                            a)))))))
  nil)

(def-op-function (vm stream (slct-fb #x0D))
  (let ((fb-id (fetch-byte stream))
        (frame (vm-frame vm)))
    (format *debug-io* "SLCT-FB fb-id=~D~%" fb-id)
    (setf (awcl-currfb-1 frame) (awcl-get-fb frame fb-id)))
  nil)

(def-op-function (vm stream (fill-fb #x0E))
  (let ((fb-id (fetch-byte stream))
        (color (fetch-byte stream)))
    (format *debug-io* "FILL-FB fb-id=~D color=0x~X~%" fb-id color)
    (awcl-fill-fb (vm-frame vm) fb-id color))
  nil)

(def-op-function (vm stream (copy-fb #x0F))
  (let ((src-fb (fetch-byte stream))
        (dst-fb (fetch-byte stream)))
    (format *debug-io* "COPY-FB src-fb=~D dst-fb=~D~%" src-fb dst-fb)
    (awcl-copy-fb (vm-frame vm) src-fb dst-fb (aref (vm-variables vm)
                                                    +vm-variable-scroll-y+)))
  nil)

(defun get-time ()
  (/ (coerce (get-internal-run-time) 'double-float)
     (coerce internal-time-units-per-second 'double-float)))

(def-op-function (vm stream (blit-fb #x10))
  (let* ((fb-id (fetch-byte stream))
         (delay (- (get-time) (vm-last-time-stamp vm)))
         (variables (vm-variables vm))
         (time-to-sleep (- (* (aref variables +vm-variable-pause-slices+) 20 0.001d0)
                           delay)))
    (format *debug-io* "BLIT-FB fb-id=~D~%" fb-id)
    (when (> time-to-sleep 0)
      (sleep time-to-sleep))
    (setf (vm-last-time-stamp vm) (get-time)
          (aref variables #xF7) 0)
    (awcl-update-display (vm-frame vm) fb-id))
  nil)

(def-op-function (vm stream (kill-channel #x11))
  (declare (ignore vm stream))
  (format *debug-io* "kill-channel~%")
  ;;(setf (channel-pc-offset (vm-channels vm)) #xFFFF)
  t)

(def-op-function (vm stream (draw-text #x12))
  (declare (ignore vm))
  (let ((string-id (fetch-word stream))
        (x (fetch-byte stream))
        (y (fetch-byte stream))
        (color (fetch-byte stream)))
    (format *debug-io* "DRAW-TEXT string-id=~D x=~D y=~D color=0x~X~%"
            string-id x y color))
  nil)

(def-op-function (vm stream (sub #x13))
  (let ((dst-var (fetch-byte stream))
        (src-var (fetch-byte stream))
        (variables (vm-variables vm)))
    (format *debug-io* "SUB dst-var=~D src-var=~D~%" dst-var src-var)
    (decf (aref variables dst-var) (aref variables src-var)))
  nil)

(def-op-function (vm stream (and #x14))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "AND var-id=~D value=0x~X~%" var-id value)
    (setf (aref variables var-id) (logand (aref variables var-id) value)))
  nil)

(def-op-function (vm stream (or #x15))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "OR var-id=~D value=0x~X~%" var-id value)
    (setf (aref variables var-id) (logior (aref variables var-id) value)))
  nil)

(def-op-function (vm stream (shl #x16))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "SHL var-id=~D value=0x~X~%" var-id value)
    (setf (aref variables var-id) (ash (aref variables var-id) value)))
  nil)

(def-op-function (vm stream (shr #x17))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "SHR var-id=~D value=0x~X~%" var-id value)
    (setf (aref variables var-id) (ash (aref variables var-id) (- value))))
  nil)

(def-op-function (vm stream (play-sound #x18))
  (declare (ignore vm))
  (let ((res-id (fetch-word stream))
        (freq (fetch-byte stream))
        (vol (fetch-byte stream))
        (channel (fetch-byte stream)))
    (format *debug-io* "PLAY-SOUND res-id=~D freq=~D vol=~D channel=~D~%"
            res-id freq vol channel))
  nil)

(def-op-function (vm stream (load-resc #x19))
  (let ((res-id (fetch-word stream)))
    (format *debug-io* "LOAD-RESC res-id=~D~%" res-id)
    (if (zerop res-id)
        t
        (progn
          (if (< res-id (length (rm-memlist (vm-resource-manager vm))))
              (rm-load-memory-entry (vm-resource-manager vm) res-id)
              (rm-load-part (vm-resource-manager vm) res-id))
          nil))))

(def-op-function (vm stream (play-music #x1A))
  (declare (ignore vm))
  (let ((res-id (fetch-word stream))
        (delay (fetch-word stream))
        (pos (fetch-byte stream)))
    (format *debug-io* "PLAY-MUSIC res-id=~D delay=~D pos=~D~%"
            res-id delay pos))
  nil)

;;;;
