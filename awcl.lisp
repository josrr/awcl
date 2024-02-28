;;;; awcl.lisp

(in-package #:awcl)

(defparameter *memlist-bin-path* #p"./data/aw01/MEMLIST.BIN")
(defparameter *canvas-width* 800)
(defparameter *canvas-height* 600)
(defparameter *frame* nil)

(defclass canvas-pane (clim-stream-pane clime:never-repaint-background-mixin)
  ())

(defmethod initialize-instance :after ((pane canvas-pane) &key contents)
  (declare (ignore contents))
  (setf (stream-recording-p pane) nil)
  t)

(define-application-frame awcl ()
  ((vm :initform nil :accessor awcl-vm)
   (fb-size :initform (/ (* *canvas-width* *canvas-height*) 2)
            :accessor awcl-fb-size)
   (fb-list :accessor awcl-fb-list)
   (currfb-1 :initform nil :accessor awcl-currfb-1)
   (currfb-2 :initform nil :accessor awcl-currfb-2)
   (currfb-3 :initform nil :accessor awcl-currfb-3)
   (palette-id :initform 0 :accessor awcl-palette-id)
   (palette-id-requested :initform nil :accessor awcl-palette-id-requested)
   (palette :initform (make-array 16 :element-type t;'clim:color
                                     :initial-element clim:+black+)
            :type (simple-array t)
            :accessor awcl-palette)
   (psudopalette :initform (make-array 16
				       :element-type 'clim:color
				       :initial-contents
				       (list clim:+grey1+ clim:+grey2+ clim:+grey3+ clim:+grey4+
					     clim:+grey5+ clim:+grey6+ clim:+grey7+ clim:+grey8+
					     clim:+grey9+ clim:+grey10+ clim:+grey11+ clim:+grey12+
					     clim:+grey13+ clim:+grey14+ clim:+grey15+ clim:+grey16+))
		 :accessor awcl-pseudopalette)
   (polygon-cache :initform nil :accessor awcl-polygon-cache)
   (palette-table :initform (make-hash-table :size 16)
            :accessor awcl-palette-table)
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

(defun run ()
  (find-application-frame 'awcl))

(defmethod run-frame-top-level :before ((frame awcl) &key &allow-other-keys)
  (let*  ((pane (first (frame-current-panes frame)))
          (pixmaps (loop repeat 4
                         collect (allocate-pixmap pane *canvas-width* *canvas-height*))))
    (setf (awcl-vm frame) (vm-create *memlist-bin-path* frame)
          (awcl-fb-list frame) (loop for i from 0 below 4
                                     for medium = (make-medium (port pane) pane)
                                     do (setf (medium-drawable medium)
                                              (elt pixmaps i))
                                     collect medium)
          (awcl-currfb-3 frame) (awcl-get-fb frame 1)
          (awcl-currfb-2 frame) (awcl-get-fb frame 2)
          (awcl-currfb-1 frame) (awcl-get-fb frame #xFE)))
  (vm-change-part (awcl-vm frame) +game-part-2+)
  (setf (awcl-palette-id-requested frame) 1))

#|(loop with palette-table = (awcl-palette-table frame)
        and pattern = (clime:pattern-array (awcl-currpix-2 frame))
	for i from 0 below (* *canvas-width* *canvas-height*)
	do (setf (row-major-aref pattern i)
		 (gethash (row-major-aref pattern i) palette-table)))|#

(defun awcl-update-canvas (frame)
  (copy-from-pixmap (medium-drawable (awcl-currfb-2 frame)) 0 0
                    *canvas-width* *canvas-height*
                    (first (frame-current-panes frame)) 0 0))

(defun awcl-get-fb (frame fb-id)
  (if (<= fb-id 3)
      (elt (awcl-fb-list frame) fb-id)
      (case fb-id
        (#xFF (awcl-currfb-3 frame))
        (#xFE (awcl-currfb-2 frame))
        (t (first (awcl-fb-list frame))))))

(defun awcl-fill-fb (frame fb-id color)
  (declare (optimize (speed 3)))
  (let ((fb (awcl-get-fb frame fb-id)))
    (declare (type fixnum color))
    (format *debug-io* "~4tcolor ~s~%" color)
    (with-bounding-rectangle* (x0 y0 x1 y1) (medium-sheet fb)
      (clim:draw-rectangle* fb x0 y0 x1 y1
                            :filled t
                            :ink (if (< color 16)
                                     (aref (awcl-palette frame) color)
                                     clim:+cyan+)))))

(defun awcl-copy-fb% (frame src-fb-id dst-fb-id vscroll)
  (flet ((copy-fb (src dst)
           (dotimes (i (awcl-fb-size frame))
             (setf (row-major-aref (clime:pattern-array dst) i)
                   (row-major-aref (clime:pattern-array src) i))))
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
                 (setf (row-major-aref (clime:pattern-array dst) (+ i q)) (row-major-aref (clime:pattern-array src) (+ i p))))))))
   (unless (= src-fb-id dst-fb-id)
     (if (or (>= src-fb-id #xFE)
             (= 0 (ldb (byte 1 7)
                       (setf src-fb-id (logand src-fb-id #xBF)))) )
         (copy-fb (awcl-get-fb frame src-fb-id)
                  (awcl-get-fb frame dst-fb-id))
         (copy-fb-scroll (awcl-get-fb frame (logand 3 src-fb-id))
                         (awcl-get-fb frame dst-fb-id))))))

(defun awcl-copy-fb (frame src-fb-id dst-fb-id vscroll)
  (flet ((copy-fb (src dst)
           (clim:copy-from-pixmap src 0 0
                                  (clim:pixmap-width src) (clim:pixmap-height src)
                                  dst 0 0))
         (copy-fb-scroll (src dst)
           (when (<= -199 vscroll 199)
             (let ((h 200)
                   (q 0)
                   (p 0))
               (cond ((minusp vscroll)
                      (incf h vscroll)
                      (decf p (* 160 vscroll)))
                     (t (decf h vscroll)
                        (incf q (* 160 vscroll))))
               (clim:copy-from-pixmap src 0 0
                                      (clim:pixmap-width src) h
                                      dst 0 0)))))
   (unless (= src-fb-id dst-fb-id)
     (if (or (>= src-fb-id #xFE)
             (= 0 (ldb (byte 1 7);; b1000 0000
                       (setf src-fb-id (logand src-fb-id #xBF)))) )
         (copy-fb (medium-drawable (awcl-get-fb frame src-fb-id))
                  (awcl-get-fb frame dst-fb-id))
         (copy-fb-scroll (medium-drawable (awcl-get-fb frame (logand 3 src-fb-id)))
                         (medium-drawable (awcl-get-fb frame dst-fb-id)))))))

(defun %vals->rgba (r g b &optional (a #xff))
  (declare (type (unsigned-byte 8) r g b a)
           (optimize (speed 3) (safety 0)))
  (logior (ash a 24) (ash r 16) (ash g 8) (ash b 0)))

(defun awcl-change-palette (frame)
  (flet (;; (color->int (color) (multiple-value-bind (r g b) (mcclim-render:color->octets color) (mcclim-render-internals::%vals->rgba r g b)))
	 (mkcolor (c1 c2)
           (make-rgb-color (/ (ash (logior (ash (logand c1 #x0F)  2)
                                           (ash (logand c1 #x0F) -2))
                                   2) 255.0)
                           (/ (ash (logior (ash (logand c2 #xF0) -2)
                                           (ash (logand c2 #xF0) -6))
                                   2) 255.0)
                           (/ (ash (logior (ash (logand c2 #x0F) -2)
                                           (ash (logand c2 #x0F)  2))
                                   2) 255.0))))
    (let ((pal-pos (* 32 (awcl-palette-id-requested frame)))
          (palette (awcl-palette frame))
	  ;; (pseudopalette (awcl-pseudopalette frame))
	  ;; (palette-table (awcl-palette-table frame))
          (palettes (rm-palette (vm-resource-manager (awcl-vm frame)))))
      (loop for i from 0 below 32 by 2
            for j from 0
            do (setf (aref palette j)
		     (mkcolor (aref palettes (+ i pal-pos))
                              (aref palettes (+ i pal-pos 1)))
		     ;;;;
		     ;; (gethash (color->int (aref pseudopalette j)) palette-table)
		     ;; (color->int (aref palette j))
                     ))
      (format *debug-io* "awcl-change-palette: [~s] ~s~%" pal-pos palette)))
  (setf (awcl-palette-id frame) (awcl-palette-id-requested frame)
        (awcl-palette-id-requested frame) nil))

(defun awcl-update-display (frame fb-id)
  (when (awcl-palette-id-requested frame)
    (awcl-change-palette frame))
  (when (/= fb-id #xFE)
    (cond ((= fb-id #xFF)
           (rotatef (awcl-currfb-2 frame) (awcl-currfb-3 frame)))
          (t (setf (awcl-currfb-2 frame) (awcl-get-fb frame fb-id)))))
  (when (awcl-polygon-cache frame)
    (labels ((traverse (cache)
               (loop for f = (pop cache)
                     while (and f (car f))
                     if (listp (car f))
                       do (traverse (reverse f))
                     else do (apply (car f) (cdr f)))))
      (traverse (reverse (awcl-polygon-cache frame))))
    (setf (awcl-polygon-cache frame) nil))
  (awcl-update-canvas frame))

(defclass awcl-polygon ()
  ((width :initarg :width :reader awcl-polygon-width)
   (height :initarg :height :reader awcl-polygon-height)
   (points :initarg :points :accessor awcl-polygon-points)))

(defun read-polygon (stream zoom)
  (make-instance 'awcl-polygon
                 :width (truncate (* (fetch-byte stream) zoom) 64)
                 :height (truncate (* (fetch-byte stream) zoom) 64)
                 :points (loop repeat (fetch-byte stream)
                               collect (make-point (truncate (* (fetch-byte stream) zoom) 64)
                                                   (truncate (* (fetch-byte stream) zoom) 64)))))

(defun awcl-draw-polygon-impl (frame sheet polygon color x y)
  (with-drawing-options (sheet :transformation
                               (clim:make-translation-transformation
                                (- x (/ (awcl-polygon-width polygon)
                                        2))
                                (- y (/ (awcl-polygon-height polygon)
                                        2))))
    (draw-polygon sheet (awcl-polygon-points polygon)
                  :ink (if (< color 16)
                           (aref (awcl-palette frame) color)
                           clim:+flipping-ink+)
                  :line-joint-shape :none
                  :line-cap-shape :square
                  :line-thickness 1
                  :filled t)))

(defun awcl-draw-polygon* (i stream frame sheet color zoom x y &optional branch-p)
  (let ((element (list 'awcl-draw-polygon-impl frame sheet
                       (read-polygon stream zoom)
                       (if (= 1 (ldb (byte 1 7) color))
                           (logand i #x3F)
                           color)
                       x y)))
    (if branch-p element (push element
                               (awcl-polygon-cache frame)))))

(defun awcl-draw-polygon-hierarchy (frame zoom x y &optional branch-p)
  (loop with cache-branch = (list)
        and fb = (awcl-currfb-1 frame)
        and stream = (vm-current-stream (awcl-vm frame))
        with ptx = (- x (truncate (* (fetch-byte stream) zoom) 64))
        and pty = (- y (truncate (* (fetch-byte stream) zoom) 64))
        and num-childs = (fetch-byte stream)
        initially (format *debug-io* "awcl-draw-polygon-hierarchy: ~3d ~3d (~d) [~d]~%"
                          x y num-childs zoom)
        repeat num-childs
        for color = #xFF
        and offset = (fetch-word stream)
        and pox = (+ ptx (floor (* (fetch-byte stream) zoom) 64))
        and poy = (+ pty (floor (* (fetch-byte stream) zoom) 64))
        if (= 1 (ldb (byte 1 15) offset))
          do (setf color (logand (fetch-byte stream) #x7F))
             (fetch-byte stream)
        end
        do (let ((pos (file-position stream))
                 (elemento (awcl-draw-polygon frame (* 2 (logand offset #x7FFF))
                                              color zoom pox poy t)))
             (when elemento (push elemento cache-branch))
             (file-position stream pos))
        finally (return (if branch-p
                            cache-branch
                            (prog1 nil
                              (push cache-branch (awcl-polygon-cache frame)))))))

;;(format *debug-io* "awcl-draw-polygon: ~s%" (awcl-polygon-points (read-polygon cinestream zoom)))
;;(if (rm-use-seg-video2 rm) (rm-seg-video2 rm) (rm-cinematic-stream rm))
(defun awcl-draw-polygon (frame offset color zoom x y &optional branch-p)
  (with-accessors ((rm vm-resource-manager)
                   (stream vm-current-stream)) (awcl-vm frame)
    (file-position stream offset)
    (let ((i (fetch-byte stream)))
      (if (>= i #xC0)
          (awcl-draw-polygon* i stream frame (awcl-currfb-1 frame)
                              color zoom x y branch-p)
          (case (logand i #x3F)       ; 63
            (1 (format *debug-io* "awcl-draw-polygon*: ec=~X (i != 2)~%" #xF80))
            (2 (awcl-draw-polygon-hierarchy frame zoom x y branch-p))
            (t (format *debug-io* "awcl-draw-polygon*: ec=~X (i != 2)~%" #xFBB)))))))

(defun redraw-canvas (frame &optional gadget-arg)
  t)

(defun make-play-thread (frame)
  (clim-sys:make-process
   (lambda ()
     (loop initially (format *debug-io* "New game!~%")
           while (awcl-playing frame)
           do (vm-run (awcl-vm frame))
           finally (format *debug-io* "Game stopped!~%")))))

(define-awcl-command (com-continue :name "Continue game") ()
  (when *application-frame*
    (setf (awcl-playing *application-frame*) t)))

(define-awcl-command (com-new :name "New game") ()
  (a:when-let ((frame *application-frame*))
    (setf (awcl-playing frame) nil
          (awcl-vm frame) (vm-create *memlist-bin-path* frame)
          ;;(awcl-palette-id-requested frame) 1
          )
    (vm-change-part (awcl-vm frame) +game-part-2+)
    ;;(awcl-change-palette frame)
    (setf (awcl-play-thread frame) (make-play-thread frame)
          (awcl-playing frame) t)))

(define-awcl-command (com-stop :name "Stop game") ()
  (when *application-frame*
    (setf (awcl-playing *application-frame*) nil)))

(define-awcl-command (com-pause :name "Pause game") ()
  (when *application-frame*
    ;;(break)
    (if (awcl-playing *application-frame*)
        (execute-frame-command *application-frame* `(com-stop))
        (execute-frame-command *application-frame* `(com-continue)))))

(define-awcl-command (com-run-frame :name "One frame") ()
  (when *application-frame*
    (vm-run (awcl-vm *application-frame*))))

(defmethod handle-event ((gadget canvas-pane) (event key-press-event))
  (format *debug-io* "event:~S ~S!~%" event *application-frame*)
  (case (keyboard-event-key-name event)
    ((:Q :|q|) (execute-frame-command *application-frame* `(com-stop)))
    ((:N :|n|) (execute-frame-command *application-frame* `(com-new)))
    ((:P :|p|) (execute-frame-command *application-frame* `(com-pause)))
    ((:space) (unless (awcl-playing *application-frame*)
                (execute-frame-command *application-frame* `(com-run-frame))))))
