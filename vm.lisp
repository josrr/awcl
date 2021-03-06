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
  (and (channel-state-current (channel-state channel))
       (/= (channel-pc-offset channel) +vm-inactive-channel+)))

(defstruct vm
  ;;(memlist nil :type (or null (simple-array (or null mem-entry) *)))
  (fast-mode nil :type boolean)
  (resource-manager nil :type resource-manager)
  ;;(resources nil :type list)
  (num-variables *num-variables* :type fixnum)
  (num-channels *num-channels* :type fixnum)
  (variables nil :type (or null (simple-array (signed-byte 16) *)))
  (script-stream nil)
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
                     :script-stream nil
                     :channels (make-array *num-channels*
                                           :initial-contents (loop for i from 0 below *num-channels*
                                                                   collect (make-instance 'channel :id i))
                                           :element-type 'channel))))
    (rm-setup-part (vm-resource-manager vm) +game-part-1+)
    vm))

(defun check-thread-requests (vm)
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

(defun run-channel (vm)
  (loop with stop = nil
        for opcode = (binary-types:read-binary 'binary-types:u8
                                               (rm-script-stream (vm-resource-manager vm)))
        if (> (logand opcode #x80) 0 ) do
          (format *debug-io* "[and #x80]  opcode=~4X  ⸺  ~2X ~2X ~2X~%" opcode
                  (binary-types:read-binary 'binary-types:u8 (vm-script-stream vm))
                  (binary-types:read-binary 'binary-types:u8 (vm-script-stream vm))
                  (binary-types:read-binary 'binary-types:u8 (vm-script-stream vm)))
        else if (> (logand opcode #x40) 0) do
          (format *debug-io* "[and #x40]  opcode=~4X  ⸺  ~%" opcode)
          (let ((off (* 2 (binary-types:read-binary 'binary-types:u16 (vm-script-stream vm))))
                (y)
                (x (binary-types:read-binary 'binary-types:u8 (vm-script-stream vm))))
            (cond ((zerop (logand opcode #x20))
                   )
                  ((= (logand opcode #x10) 1))
                  )
            (cond ((= (logand opcode #x10) 1)
                   )
                  (t)))
        else if (> opcode #x1a) do
          (format *debug-io* "[ >  #x1a]  opcode=~4X  ⸺  ~%" opcode)
          (error (make-condition 'runtime-error :opcode opcode
                                 :position (file-position (vm-script-stream vm))))
        else do
          (setf stop (null (funcall (vm-op-function opcode))))
        end
        until stop))

#|(lambda (c)
  (and (not (getf (getf c :is-active) :curr-state))
       (/= (getf c :pc-offset) +vm-inactive-channel+)))|#

(defun run-one-frame (vm)
  (loop for channel across (remove-if-not #'channel-is-active-p
                                          (vm-channels vm))
        do (file-position (vm-script-stream vm) (channel-pc-offset channel))
           (run-channel vm)))

;;;;
(defun vm-change-part (vm part-id)
  (setf (vm-resources vm) (setup-part part-id (vm-memlist vm))
        (vm-script-stream vm) (flexi-streams:make-in-memory-input-stream
                               (resource-data (getf (vm-resources vm) :bytecode)))
        (channel-pc-offset (aref (vm-channels vm) 0)) 0)
  (loop for c across (vm-channels vm)
        do (setf (channel-pc-offset c) +vm-inactive-channel+
                 (channel-state-current (channel-state c)) nil))
  (file-position (vm-script-stream vm) 0)
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
  (check-thread-requests vm)
  (run-one-frame vm))

;;;;
(defmacro def-op-function ((vm (name opcode)) &body body)
  (let ((g-func (gensym)))
    `(let ((,g-func (lambda (,vm)
                      ,@body)))
       (setf (symbol-function (alexandria:symbolicate ',name '-op)) ,g-func
             (aref *vm-opcodes* ,opcode) ,g-func))))

(def-op-function (vm (cmov #x00))
  (declare (ignore vm))
  nil)

(def-op-function (vm (mov #x01))
  (declare (ignore vm))
  nil)

(def-op-function (vm (add #x02))
  (declare (ignore vm))
  nil)

(def-op-function (vm (cadd #x03))
  (declare (ignore vm))
  nil)

(def-op-function (vm (call #x04))
  (declare (ignore vm))
  nil)

(def-op-function (vm (ret #x05))
  (declare (ignore vm))
  nil)

(def-op-function (vm (pause-thrd #x06))
  (declare (ignore vm))
  nil)

(def-op-function (vm (cond-jmp #x07))
  (declare (ignore vm))
  nil)

(def-op-function (vm (set-vect #x08))
  (declare (ignore vm))
  nil)

(def-op-function (vm (jnz #x09))
  (declare (ignore vm))
  nil)

(def-op-function (vm (cjmp #x0A))
  (declare (ignore vm))
  nil)

(def-op-function (vm (set-pal #x0B))
  (declare (ignore vm))
  nil)

(def-op-function (vm (reset-thrd #x0C))
  (declare (ignore vm))
  nil)

(def-op-function (vm (slct-fb #x0D))
  (declare (ignore vm))
  nil)

(def-op-function (vm (fill-fb #x0E))
  (declare (ignore vm))
  nil)

(def-op-function (vm (copy-fb #x0F))
  (declare (ignore vm))
  nil)

(def-op-function (vm (blit-fb #x10))
  (declare (ignore vm))
  nil)

(def-op-function (vm (kill-thdr #x11))
  (declare (ignore vm))
  nil)

(def-op-function (vm (draw-text #x12))
  (declare (ignore vm))
  nil)

(def-op-function (vm (sub #x13))
  (declare (ignore vm))
  nil)

(def-op-function (vm (and #x14))
  (declare (ignore vm))
  nil)

(def-op-function (vm (or #x15))
  (declare (ignore vm))
  nil)

(def-op-function (vm (shl #x16))
  (declare (ignore vm))
  nil)

(def-op-function (vm (shr #x17))
  (declare (ignore vm))
  nil)

(def-op-function (vm (play-sound #x18))
  (declare (ignore vm))
  nil)

(def-op-function (vm (load-resc #x19))
  (declare (ignore vm))
  nil)

(def-op-function (vm (play-music #x1A))
  (declare (ignore vm))
  nil)

;;;;
