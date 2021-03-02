;;;; parts.lisp

(in-package #:awcl)

(defconstant +game-part-first+ #x3e80)
(defconstant +game-part-1+ #x3e80)
(defconstant +game-part-2+ #x3e81)
(defconstant +game-part-3+ #x3e82)
(defconstant +game-part-4+ #x3e83)
(defconstant +game-part-5+ #x3e84)
(defconstant +game-part-6+ #x3e85)
(defconstant +game-part-7+ #x3e86)
(defconstant +game-part-8+ #x3e87)
(defconstant +game-part-9+ #x3e88)
(defconstant +game-part-10+ #x3e89)
(defconstant +game-part-last+ #x3e89)

(defconstant +memlist-part-palette+ 0)
(defconstant +memlist-part-code+ 1)
(defconstant +memlist-part-video-1+ 2)
(defconstant +memlist-part-video-2+ 3)
(defconstant +memlist-part-none+ 0)

(defparameter *memlist-parts*
  #((:palette #x14 :code #x15 :video-1 #x16 :video-2 #x00)
    (:palette #x17 :code #x18 :video-1 #x19 :video-2 #x00)
    (:palette #x1a :code #x1b :video-1 #x1c :video-2 #x11)
    (:palette #x1d :code #x1e :video-1 #x1f :video-2 #x11)
    (:palette #x20 :code #x21 :video-1 #x22 :video-2 #x11)
    (:palette #x23 :code #x24 :video-1 #x25 :video-2 #x00)
    (:palette #x26 :code #x27 :video-1 #x28 :video-2 #x11)
    (:palette #x29 :code #x2a :video-1 #x2b :video-2 #x11)
    (:palette #x7d :code #x7e :video-1 #x7f :video-2 #x00)
    (:palette #x7d :code #x7e :video-1 #x7f :video-2 #x00)))

;;;;
