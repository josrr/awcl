;;;; awcl.asd

(asdf:defsystem #:awcl
  :description "Describe awcl here"
  :author "J. M. R. R."
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria :binary-types :flexi-streams :mcclim :mcclim-raster-image)
  :components ((:file "package")
               (:file "resources")
               (:file "parts")
               (:file "vm")
               (:file "awcl")))
