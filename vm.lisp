;;;; vm.lisp

(in-package #:awcl)

(defparameter *num-channels* 64)
(defparameter *num-variables* 256)

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

(defstruct vm
  (memlist nil :type (or null (simple-array (or null mem-entry) *)))
  (fast-mode nil :type boolean)
  (resources nil :type list)
  (num-variables *num-variables* :type fixnum)
  (num-channels *num-channels* :type fixnum)
  (variables nil :type (or null (simple-array (unsigned-byte 16) *)))
  (script-stream nil)
  (channels nil :type (or null (simple-array list *))))

(defun vm-create (memlist-path)
  (let* ((memlist (memlist-create memlist-path))
         (resources (setup-part +game-part-1+ memlist)))
    (make-vm :memlist memlist
             :fast-mode nil
             :resources resources
             :num-variables *num-variables*
             :num-channels *num-channels*
             :variables (make-array *num-variables*
                                    :initial-element 0
                                    :element-type '(unsigned-byte 16))
             :script-stream nil
             :channels (make-array *num-channels*
                                   :initial-contents (loop for i from 0 below *num-channels*
                                                           collect (list :id i
                                                                         :pc-offset +vm-inactive-channel+
                                                                         :requested-pc-offset 0
                                                                         :is-active (list :curr-state nil
                                                                                          :requested-state nil)))
                                   :element-type 'list))))

(defparameter *vm-opcodes* (make-array #xFF :element-type 'function))

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
        for opcode = (binary-types:read-binary 'binary-types:u8 (vm-script-stream vm))
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


(defun run-one-frame (vm)
  (loop with active-channels = (remove-if (lambda (c)
                                            (and (not (getf (getf c :is-active) :curr-state))
                                                 (/= (getf c :pc-offset) +vm-inactive-channel+)))
                                          (vm-channels vm))
        for channel across active-channels
        do (file-position (vm-script-stream vm) (getf channel :pc-offset))
           (run-channel vm)))

;;;;
(defun vm-change-part (vm part-id)
  (setf (vm-resources vm)
        (setup-part part-id (vm-memlist vm)))
  (map nil (lambda (c) (setf (getf c :pc-offset) +vm-inactive-channel+
                             (getf (getf c :is-active) :curr-state) nil))
       (vm-channels vm))
  (setf (vm-script-stream vm) (flexi-streams:make-in-memory-input-stream
                               (getf (getf (vm-resources vm) :bytecode)
                                     :data))
        (getf (aref (vm-channels vm) 0) :pc-offset) 0)
  (file-position (vm-script-stream vm) 0)
  vm)

(defun vm-init (vm)
  (let ((variables (vm-variables vm)))
    (dotimes (i (vm-num-variables vm))
      (setf (aref variables i) 0))
    (setf (aref variables #x54) #x81
          (aref variables +vm-variable-random-seed+) (get-universal-time)
          ;; (player-mark-var (getf vm :player)) (aref variables +variable-mus-mark+)
          (vm-fast-mode vm) nil)))

;;;;
(defmacro def-op-function ((name opcode) &body body)
  (let ((g-opcode (gensym))
        (g-func (gensym)))
    `(let* ((,g-opcode ,opcode)
            (,g-func (defun ,(alexandria:symbolicate name '-op) () ,@body)))
       (setf (aref *vm-opcodes* ,g-opcode) ,g-func))))

(def-op-function (cmov #x00)
  nil)

(def-op-function (mov #x01)
  nil)

(def-op-function (add #x02)
  nil)

(def-op-function (cadd #x03)
  nil)

(def-op-function (call #x04)
  nil)

(def-op-function (ret #x05)
  nil)

(def-op-function (pause-thrd #x06)
  nil)

(def-op-function (cond-jmp #x07)
  nil)

(def-op-function (set-vect #x08)
  nil)

(def-op-function (jnz #x09)
  nil)

(def-op-function (cjmp #x0A)
  nil)

(def-op-function (set-pal #x0B)
  nil)

(def-op-function (reset-thrd #x0C)
  nil)

(def-op-function (slct-fb #x0D)
  nil)

(def-op-function (fill-fb #x0E)
  nil)

(def-op-function (copy-fb #x0F)
  nil)

(def-op-function (blit-fb #x10)
  nil)

(def-op-function (kill-thdr #x11)
  nil)

(def-op-function (draw-text #x12)
  nil)

(def-op-function (sub #x13)
  nil)

(def-op-function (and #x14)
  nil)

(def-op-function (or #x15)
  nil)

(def-op-function (shl #x16)
  nil)

(def-op-function (shr #x17)
  nil)

(def-op-function (play-sound #x18)
  nil)

(def-op-function (load-resc #x19)
  nil)

(def-op-function (play-music #x1A)
  nil)

;;;;
