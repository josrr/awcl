;;;; vm.lisp

(in-package #:awcl)

(defun vm-create (memlist-path)
  (let* ((memlist (memlist-create memlist-path))
         (resources (setup-part +game-part-1+ memlist)))
    (list :memlist memlist
          :resources resources)))

(defun vm-change-part (vm part-id)
  (setf (getf vm :resources)
        (setup-part part-id (getf vm :memlist)))
  vm)

;;;;
