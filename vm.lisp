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
  (channels nil :type (or null (simple-array channel))))

(defun vm-create (memlist-path)
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
                                           :element-type 'channel))))
    (vm-init vm)
    vm))

(defun check-channel-requests (vm)
  (rm-setup-next-part (vm-resource-manager vm))
  (loop for channel across (vm-channels vm)
        for offset = (channel-requested-pc-offset channel)
        do (setf (channel-state-current (channel-state channel))
                 (channel-state-requested (channel-state channel)))
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
             (format stream "Error runing bytecode opcode ~S in position ~D."
                     (runtime-error-opcode condition)
                     (runtime-error-position condition)))))

;;(declaim (inline fetch-word fetch-byte))
(defun fetch-word (stream)
  (let ((binary-types:*endian* :big-endian))
    (binary-types:read-binary 'binary-types:u16 stream)))

(defun fetch-byte (stream)
  (binary-types:read-binary 'binary-types:u8 stream))

(defun run-channel (vm)
  (loop with stop = nil
        with script-stream = (rm-script-stream (vm-resource-manager vm))
        for opcode = (fetch-byte script-stream)
        if (> (logand opcode #x80) 0 ) do
          (format *debug-io* "[and #x80]  opcode=~4X  ⸺  ~2X ~2X ~2X~%" opcode
                  (fetch-byte script-stream)
                  (fetch-byte script-stream)
                  (fetch-byte script-stream))
        else if (> (logand opcode #x40) 0) do
          (format *debug-io* "[and #x40]  opcode=~4X  ⸺  ~%" opcode)
          (let ((off (* 2 (fetch-word script-stream)))
                (y)
                (x (fetch-byte script-stream)))
            (cond ((zerop (logand opcode #x20))
                   )
                  ((= (logand opcode #x10) 1))
                  )
            (cond ((= (logand opcode #x10) 1)
                   )
                  (t)))
        else if (> opcode #x1a) do
          (format *debug-io* "[ >  #x1a]  opcode=~4X  ⸺  ~%" opcode)
          (error (make-condition 'runtime-error
                                 :opcode opcode
                                 :position (file-position script-stream)))
        else do
          (format *debug-io* "Opcode: ~S~%" opcode)
          (setf stop (funcall (vm-op-function opcode) vm script-stream))
        end
        until stop))

(defun run-one-frame (vm)
  (loop with script-stream = (rm-script-stream (vm-resource-manager vm))
        for channel across (remove-if-not #'channel-is-active-p
                                          (vm-channels vm))
        do (format *debug-io* "c: ~S~%" channel)
           (file-position script-stream (channel-pc-offset channel))
           (run-channel vm)
           (setf (channel-pc-offset channel) (file-position script-stream))))

;;;;
(defun vm-change-part (vm part-id)
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
          (aref variables +vm-variable-random-seed+) (ash (get-universal-time) -17)
          ;; (sfxplayer-mark-var (getf vm :player)) (aref variables +variable-mus-mark+)
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
        (value (fetch-word stream)))
    (format *debug-io* "CMOV var-id=~X value=~X~%" var-id value)
    (setf (aref (vm-variables vm) var-id) value))
  nil)

(def-op-function (vm stream (mov #x01))
  (let ((dst-var (fetch-byte stream))
        (src-var (fetch-byte stream)))
    (format *debug-io* "MOV dst-var=~X src-var=~X~%" dst-var src-var)
    (setf (aref (vm-variables vm) dst-var) (aref (vm-variables vm) src-var)))
  nil)

(def-op-function (vm stream (add #x02))
  (let ((dst-var (fetch-byte stream))
        (src-var (fetch-byte stream)))
    (format *debug-io* "ADD dst-var=~X src-var=~X~%" dst-var src-var)
    (incf (aref (vm-variables vm) dst-var) (aref (vm-variables vm) src-var)))
  nil)

(def-op-function (vm stream (cadd #x03))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream)))
    (format *debug-io* "CADD var-id=~X value=~X~%" var-id value)
    (incf (aref (vm-variables vm) var-id) value))
  nil)

(def-op-function (vm stream (call #x04))
  (let ((offset (fetch-word stream)))
    (format *debug-io* "CALL offset=~X~%" offset)
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
  (format *debug-io* "RET offset=~X~%" (vm-stack-pos vm))
  (file-position stream (aref (vm-stack-calls vm) (vm-stack-pos vm)))
  nil)

(def-op-function (vm stream (pause-channel #x06))
  (declare (ignore vm stream))
  (format *debug-io* "PAUSE-CHANNEL~%")
  t)

(def-op-function (vm stream (jmp #x07))
  (declare (ignore vm))
  (let ((offset (fetch-word stream)))
    (format *debug-io* "JMP offset=~X~%" offset)
    (file-position stream offset))
  nil)

(def-op-function (vm stream (set-vect #x08))
  (let ((channel-id (fetch-byte stream))
        (offset (fetch-word stream)))
    (format *debug-io* "SET-VECT channel-id=~X  offset=~X~%"
            channel-id offset)
    (setf (channel-requested-pc-offset (aref (vm-channels vm) channel-id))
          offset))
  nil)

(def-op-function (vm stream (jnz #x09))
  (let ((var-id (fetch-byte stream)))
    (format *debug-io* "JNZ var-id=~X~%" var-id)
    (decf (aref (vm-variables vm) var-id))
    (if (zerop (aref (vm-variables vm) var-id))
        (jmp-op vm stream)
        (fetch-word stream)))
  nil)

(def-op-function (vm stream (cond-jmp #x0A))
  (let* ((opcode (fetch-byte stream))
         (condition (logand opcode #x7))
         (b (aref (vm-variables vm) (fetch-byte stream)))
         (a (cond
              ((> (logand opcode #x80) 0)
               (aref (vm-variables vm) (fetch-byte stream)))
              ((> (logand opcode #x40) 0)
               (fetch-word stream))
              (t (fetch-byte stream))))
         (func (case condition
                 (0 #'=)
                 (1 #'/=)
                 (2 #'>)
                 (3 #'<)
                 (4 #'<=)
                 (5 #'>=)
                 (otherwise nil))))
    (format *debug-io* "COND-JMP opcode=~X condition=~X func=~S~%"
            opcode condition func)
    (if func
      (if (funcall func b a)
          (jmp-op vm stream)
          (fetch-word stream))
      (warn "cond-jmp-op: invalid condition ~d" condition)))
  nil)

(def-op-function (vm stream (set-pal #x0B))
  (declare (ignore vm stream))
  (format *debug-io* "SET-PAL~%")
  nil)

(def-op-function (vm stream (reset-channel #x0C))
  (let* ((channel-id (fetch-byte stream))
         (i (a:clamp (fetch-byte stream) 0 (1- *num-channels*))))
    (format *debug-io* "RESET-CHANNEL channel-id=~X i=~X~%" channel-id i)
    (if (> i channel-id)
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
  (declare (ignore vm))
  (let ((fb-id (fetch-byte stream)))
   (format *debug-io* "SLCT-FB fb-id=~X~%" fb-id))
  nil)

(def-op-function (vm stream (fill-fb #x0E))
  (declare (ignore vm))
  (let ((fb-id (fetch-byte stream))
        (color (fetch-byte stream)))
    (format *debug-io* "FILL-FB fb-id=~X color=~X~%" fb-id color))
  nil)

(def-op-function (vm stream (copy-fb #x0F))
  (declare (ignore vm))
  (let ((src-fb (fetch-byte stream))
        (dst-fb (fetch-byte stream)))
    (format *debug-io* "COPY-FB src-fb=~X dst-fb=~X~%" src-fb dst-fb))
  nil)

(def-op-function (vm stream (blit-fb #x10))
  (declare (ignore vm))
  (let ((fb-id (fetch-byte stream)))
    (format *debug-io* "BLIT-FB fb-id=~X~%" fb-id))
  nil)

(def-op-function (vm stream (kill-channel #x11))
  (declare (ignore vm))
  (file-position stream #xFFFF)
  t)

(def-op-function (vm stream (draw-text #x12))
  (declare (ignore vm))
  (let ((string-id (fetch-word stream))
        (x (fetch-byte stream))
        (y (fetch-byte stream))
        (color (fetch-byte stream)))
    (format *debug-io* "DRAW-TEXT string-id=~X x=~X y=~X color=~X~%"
            string-id x y color))
  nil)

(def-op-function (vm stream (sub #x13))
  (let ((dst-var (fetch-byte stream))
        (src-var (fetch-byte stream))
        (variables (vm-variables vm)))
    (format *debug-io* "SUB dst-var=~X src-var=~X~%" dst-var src-var)
    (decf (aref variables dst-var) (aref variables src-var)))
  nil)

(def-op-function (vm stream (and #x14))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "AND var-id=~X value=~X~%" var-id value)
    (setf (aref variables var-id) (logand (aref variables var-id) value)))
  nil)

(def-op-function (vm stream (or #x15))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "OR var-id=~X value=~X~%" var-id value)
    (setf (aref variables var-id) (logior (aref variables var-id) value)))
  nil)

(def-op-function (vm stream (shl #x16))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "SHL var-id=~X value=~X~%" var-id value)
    (setf (aref variables var-id) (ash (aref variables var-id) value)))
  nil)

(def-op-function (vm stream (shr #x17))
  (let ((var-id (fetch-byte stream))
        (value (fetch-word stream))
        (variables (vm-variables vm)))
    (format *debug-io* "SHR var-id=~X value=~X~%" var-id value)
    (setf (aref variables var-id) (ash (aref variables var-id) (- value))))
  nil)

(def-op-function (vm stream (play-sound #x18))
  (declare (ignore vm))
  (let ((res-id (fetch-word stream))
        (freq (fetch-byte stream))
        (vol (fetch-byte stream))
        (channel (fetch-byte stream)))
    (format *debug-io* "PLAY-SOUND res-id=~X freq=~X vol=~X channel=~X~%"
            res-id freq vol channel))
  nil)

(def-op-function (vm stream (load-resc #x19))
  (declare (ignore vm))
  (let ((res-id (fetch-word stream)))
    (format *debug-io* "LOAD-RESC res-id=~X~%" res-id)
    #|(if (zerop res-id)
        (progn
          t)
        (vm-resource-manager vm))|#)
  nil)

(def-op-function (vm stream (play-music #x1A))
  (declare (ignore vm))
  (let ((res-id (fetch-word stream))
        (delay (fetch-byte stream))
        (pos (fetch-byte stream)))
    (format *debug-io* "PLAY-MUSIC res-id=~X delay=~X pos=~X~%"
            res-id delay pos))
  nil)

;;;;
