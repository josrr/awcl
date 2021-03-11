;;;; awcl.lisp

(in-package #:awcl)

(defparameter *memlist-bin-path* #p"./data/aw01/MEMLIST.BIN")
(defparameter *canvas-width* 320)
(defparameter *canvas-height* 200)
(defparameter *frame* nil)

(defclass canvas-pane (clim-stream-pane clime:never-repaint-background-mixin)
  ())

(define-application-frame awcl ()
  ((width :initform *canvas-width*)
   (height :initform *canvas-height*)
   (vm :initform nil :accessor awcl-vm)
   (fb-array :initform (make-array 4 :initial-contents
                                   (loop repeat 4
                                         collect (make-image *canvas-width*
                                                             *canvas-height*)))
             :accessor awcl-fb-array)
   (currfb-1 :initform nil :accessor awcl-currfb-1)
   (currfb-2 :initform nil :accessor awcl-currfb-2)
   (currfb-3 :initform nil :accessor awcl-currfb-3)
   (play-thread :initform nil :accessor awcl-play-thread)
   (playing :initform nil :accessor awcl-playing))
  (:pane (make-pane 'canvas-pane
                    :background +black+
                    :name 'canvas
                    :width *canvas-width*
                    :height *canvas-height*
                    :max-width *canvas-width*
                    :max-height *canvas-height*
                    :min-width *canvas-width*
                    :min-height *canvas-height*
                    :display-time nil
                    :display-function 'redraw-canvas)))

(defun awcl-get-fb (frame fb-id)
  (if (<= fb-id 3)
      (aref (awcl-fb-array frame) fb-id)
      (case fb-id
        (#xFF (awcl-currfb-3 frame))
        (#xFE (awcl-currfb-2 frame))
        (t (aref (awcl-fb-array frame) 0)))))

(defun awcl-fill-page (frame fb-id color)
  (let* ((fb (awcl-get-fb frame fb-id))
         (pixels (clime:pattern-array fb))
         (color (logior (ash color 4) color)))
    (dotimes (y *canvas-height*)
      (dotimes (x *canvas-width*)
        (setf (aref pixels y x) color)))))

(defun run ()
  (setf *frame* (make-application-frame 'awcl)
        (awcl-vm *frame*) (vm-create *memlist-bin-path* *frame*)
        (awcl-currfb-3 *frame*) (awcl-get-fb *frame* 1)
        (awcl-currfb-2 *frame*) (awcl-get-fb *frame* 2)
        (awcl-currfb-1 *frame*) (awcl-get-fb *frame* #xFE))
  (vm-change-part (awcl-vm *frame*) +game-part-2+)
  (run-frame-top-level *frame*))

(defun redraw-canvas (frame &optional gadget-arg)
  t)

(defun make-play-thread (frame)
  (clim-sys:make-process
   (lambda ()
     (loop initially (format *debug-io* "New game!~%")
           while (awcl-playing frame)
           do (vm-run (awcl-vm frame))
           finally (format *debug-io* "Game stopped!~%")))))

(define-awcl-command (com-new :name "New game") ()
  (when *frame*
    (unless (awcl-playing *frame*)
      (setf (awcl-play-thread *frame*) (make-play-thread *frame*)
            (awcl-playing *frame*) t))))

(define-awcl-command (com-stop :name "Stop game") ()
  (when *frame*
    (setf (awcl-playing *frame*) nil)))

(define-awcl-command (com-run-frame :name "One frame") ()
  (when *frame*
    (vm-run (awcl-vm *frame*))))

(defmethod handle-event ((gadget canvas-pane) (event key-press-event))
  (format *debug-io* "event:~S ~S!~%" event *frame*)
  (case (keyboard-event-key-name event)
    ((:Q :|q|) (execute-frame-command *frame* `(com-stop)))
    ((:N :|n|) (execute-frame-command *frame* `(com-new)))
    ((:| |) (execute-frame-command *frame* `(com-run-frame)))))
