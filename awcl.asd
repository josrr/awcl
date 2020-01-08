;;;; awcl.asd

(asdf:defsystem #:awcl
  :description "Describe awcl here"
  :author "J. M. R. R."
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (:binary-types)
  :components ((:file "package")
               (:file "awcl")
               (:file "resources")
               (:file "parts")
               (:file "vm")))
