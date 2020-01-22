;;;; vm.lisp

(in-package #:awcl)

(defparameter *num-channels* 64)
(defparameter *num-variables* 256)

(defconstant +vm-inactive-channel+ #xFFFF)

(defun vm-create (memlist-path)
  (let* ((memlist (memlist-create memlist-path))
         (resources (setup-part +game-part-1+ memlist)))
    (list :memlist memlist
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

(defparameter *vm-opcodes* (list))

(defun vm-op-function (opcode &optional (opcodes *vm-opcodes*))
  (or (cdr (assoc opcode opcodes))
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
        for opcode = (binary-types:read-binary binary-types:u8 (getf vm :script-stream))
        if (> (logand opcode #x80) 0 ) do
          (format *debug-io* "[and #x80]  opcode=~4X  ⸺  ~2X ~2X ~2X~%" opcode
                  (binary-types:read-binary binary-types:u8 (getf vm :script-stream))
                  (binary-types:read-binary binary-types:u8 (getf vm :script-stream))
                  (binary-types:read-binary binary-types:u8 (getf vm :script-stream)))
        else if (> (logand opcode #x40) 0) do
          (format *debug-io* "[and #x40]  opcode=~4X  ⸺  ~%" opcode)
        else if (> opcode #x1a) do
          (format *debug-io* "[ >  #x1a]  opcode=~4X  ⸺  ~%" opcode)
          (error (make-condition 'runtime-error :opcode opcode
                                 :position (file-position (getf vm :script-stream))))
        else do
          (setf stop (null (funcall (vm-op-function opcode))))
        end
        until stop))

(defun run-one-frame (vm)
  (loop with active-channels = (remove-if (lambda (c)
                                            (and (not (getf (getf c :is-active) :curr-state))
                                                 (/= (getf c :pc-offset) +vm-inactive-channel+)))
                                          (getf vm :channels))
        and bytecode = (getf (getf (getf vm :resources) :bytecode) :data)
        for channel across active-channels
        do (setf (file-position (getf vm :script-stream) 0)
                 (getf channel :pc-offset))
           (run-channel vm bytecode)))

;;;;
(defun vm-change-part (vm part-id)
  (setf (getf vm :resources)
        (setup-part part-id (getf vm :memlist)))
  (map nil (lambda (c) (setf (getf c :pc-offset) +vm-inactive-channel+
                             (getf (getf c :is-active) :curr-state) nil))
       (getf vm :channels))
  (setf (getf vm :script-stream) (flexi-streams:make-in-memory-input-stream
                                  (getf (getf (getf vm :resources) :bytecode)
                                        :data))
        (getf (aref (getf vm :channels) 0) :pc-offset) 0)
  (file-position (getf vm :script-stream) 0)
  vm)

;;;;
(defmacro def-op-function ((name opcode) &body body)
  (let ((g-opcode (gensym))
        (g-cons (gensym))
        (g-func (gensym)))
    `(let* ((,g-opcode ,opcode)
            (,g-cons (assoc ,g-opcode *vm-opcodes*))
            (,g-func (defun ,name () ,@body)))
       (if ,g-cons
           (rplacd ,g-cons ,g-func)
           (setf *vm-opcodes* (cons (cons ,g-opcode ,g-func)
                                    *vm-opcodes*))))))

(def-op-function (pause-thread #x04)
  nil)

;;;;
