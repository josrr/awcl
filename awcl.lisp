;;;; awcl.lisp

(in-package #:awcl)

(defparameter *memlist-bin-path* #p"./data/aw01/MEMLIST.BIN")
(defparameter *canvas-width* 320)
(defparameter *canvas-height* 200)
(defparameter *frame* nil)

(defclass canvas-pane (clim-stream-pane clime:never-repaint-background-mixin)
  ())

(define-application-frame awcl ()
  (;;(width :initform *canvas-width*)
   ;;(height :initform *canvas-height*)
   (vm :initform nil :accessor awcl-vm)
   (fb-size :initform (/ (* *canvas-width* *canvas-height*) 2)
            :accessor awcl-fb-size)
   (fb-list :initform (loop repeat 4
                            collect (make-array (/ (* *canvas-width*
                                                      *canvas-height*)
                                                   2)
                                                :element-type '(unsigned-byte 8)
                                                :initial-element 0))
            :accessor awcl-fb-list)
   (currfb-1 :initform nil :accessor awcl-currfb-1)
   (currfb-2 :initform nil :accessor awcl-currfb-2)
   (currfb-3 :initform nil :accessor awcl-currfb-3)
   (palette-id :initform 0 :accessor awcl-palette-id)
   (palette-id-requested :initform nil :accessor awcl-palette-id-requested)
   (palette :initform (make-array 16 :element-type '(unsigned-byte 32)
                                     :initial-element 0)
            :accessor awcl-palette)
   (image :initform (make-image *canvas-width* *canvas-height*)
          :accessor awcl-image)
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

(defun awcl-update-canvas (frame)
  (let ((pixels (clime:pattern-array (awcl-image frame)))
        (fb (awcl-currfb-2 frame))
        (w/2 (/ *canvas-width* 2))
        (canvas (first (frame-current-panes frame)))
        (palette (awcl-palette frame)))
    (dotimes (y *canvas-height*)
      (dotimes (x w/2)
        (let ((fb-val (aref fb (+ x (* y w/2)))))
          (setf (aref pixels y (* 2 x)) (aref palette (ash fb-val -4))
                (aref pixels y (1+ (* 2 x))) (aref palette (logand fb-val #xF))))))
    (draw-design canvas (awcl-image frame) :x 0 :y 0)))

(defun awcl-get-fb (frame fb-id)
  (if (<= fb-id 3)
      (elt (awcl-fb-list frame) fb-id)
      (case fb-id
        (#xFF (awcl-currfb-3 frame))
        (#xFE (awcl-currfb-2 frame))
        (t (aref (awcl-fb-list frame) 0)))))

(defun awcl-fill-fb (frame fb-id color)
  (declare (optimize (speed 3)))
  (let* ((fb (awcl-get-fb frame fb-id))
         (color (logior (ash color 4) color)))
    (dotimes (i (the fixnum (awcl-fb-size frame)))
      (setf (aref fb i) color))))

(defun awcl-copy-fb (frame src-fb-id dst-fb-id vscroll)
  (flet ((copy-fb (src dst)
           (dotimes (i (awcl-fb-size frame))
             (setf (aref dst i) (aref src i))))
         (copy-fb-scroll (src dst)
           (when (and (>= vscroll -199) (<= vscroll 199))
             (let ((h 200)
                   (q 0)
                   (p 0))
               (if (minusp vscroll)
                   (progn (incf h vscroll)
                          (setf p (* -160 vscroll)))
                   (progn (decf h vscroll)
                          (setf q (* 160 vscroll))))
               (dotimes (i (* h 160))
                 (setf (aref dst (+ i q)) (aref src (+ i p))))))))
   (unless (= src-fb-id dst-fb-id)
     (if (or (>= src-fb-id #xFE)
             (= 0 (ldb (byte 1 7)
                       (setf src-fb-id (logand src-fb-id #xBF)))) )
         (copy-fb (awcl-get-fb frame src-fb-id)
                  (awcl-get-fb frame dst-fb-id))
         (copy-fb-scroll (awcl-get-fb frame (logand 3 src-fb-id))
                         (awcl-get-fb frame dst-fb-id))))))

(defun %vals->rgba (r g b &optional (a #xff))
  (declare (type (unsigned-byte 8) r g b a)
           (optimize (speed 3) (safety 0)))
  (logior (ash a 24) (ash r 16) (ash g 8) (ash b 0)))

(defun awcl-update-display (frame fb-id)
  (when (/= fb-id #xFE)
    (if (= fb-id #xFF)
        (psetf (awcl-currfb-2 frame) (awcl-currfb-3 frame)
               (awcl-currfb-3 frame) (awcl-currfb-2 frame))
        (setf (awcl-currfb-2 frame) (awcl-get-fb frame fb-id))))
  (when (awcl-palette-id-requested frame)
    (let ((pal-pos (* 32 (awcl-palette-id-requested frame)))
          (palette (awcl-palette frame))
          (palettes (rm-palette (vm-resource-manager (awcl-vm frame)))))
      (dotimes (i 16)
        (let ((c1 (aref palettes (+ i pal-pos)))
              (c2 (aref palettes (+ i pal-pos 1))))
          (setf (aref palette i) (%vals->rgba (ash (logior (ash (logand c1 #x0F) 2)
                                                           (ash (logand c1 #x0F) -2))
                                                   2)
                                              (ash (logior (ash (logand c2 #xF0) -2)
                                                           (ash (logand c2 #xF0) -6))
                                                   2)
                                              (ash (logior (ash (logand c2 #x0F) -2)
                                                           (ash (logand c2 #x0F) 2))
                                                   2))))))
    (setf (awcl-palette-id frame) (awcl-palette-id-requested frame)
          (awcl-palette-id-requested frame) nil))
  (awcl-update-canvas frame))

(defclass awcl-polygon ()
  ((width :initarg :width :reader awcl-polygon-width)
   (height :initarg :height :reader awcl-polygon-height)
   (points :initarg :points :accessor awcl-polygon-points)))

(defun read-polygon (stream zoom)
  (let ((poly (make-instance 'awcl-polygon
                             :width (/ (* (fetch-byte stream) zoom) 64)
                             :height (/ (* (fetch-byte stream) zoom) 64)))
        (num-points (fetch-byte stream)))
    ;;(make-polygon* )
    (setf (awcl-polygon-points poly)
          (loop repeat num-points
                collect (make-point (/ (* (fetch-byte stream) zoom) 64)
                                    (/ (* (fetch-byte stream) zoom) 64))))
    poly))

(defun awcl-draw-polygon (frame offset color zoom x y)
  (let ((stream (rm-cinematic-stream (vm-resource-manager (awcl-vm frame)))))
    (file-position stream offset)
    (let ((i (fetch-byte stream)))
      (cond
        ((> i #xC0) ; 192
         (when (ldb (byte 1 7) color)
           (setf color (logand i #x3F)))
         (let ((polygon (read-polygon stream zoom)))
           (format *debug-io* "~S~%" polygon)))
        (t
         t)))))

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
