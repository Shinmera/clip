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
  :version "0.2.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An HTML templating engine using Plump."
  :homepage "https://github.com/Shinmera/clip"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "clipboard")
               (:file "attr-processors")
               (:file "tag-processors")
               (:file "processor"))
  :depends-on (:lquery))
