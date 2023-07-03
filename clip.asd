(defsystem clip
  :name "Clip HTML Processor"
  :version "0.7.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An HTML templating engine using Plump."
  :homepage "https://Shinmera.github.io/clip/"
  :bug-tracker "https://github.com/Shinmera/clip/issues"
  :source-control (:git "https://github.com/Shinmera/clip.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "clipboard")
               (:file "attr-processors")
               (:file "tag-processors")
               (:file "processor"))
  :depends-on (:array-utils
               :lquery))
