#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.clip.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.clip.asdf)

(defsystem clip
  :name "Clip HTML Processor"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A powerful HTML templating engine using CXML."
  :serial T
  :components ((:file "package")
               (:file "processor")
               (:file "default-blocks"))
  :depends-on (:cxml
               :alexandria
               :split-sequence))
