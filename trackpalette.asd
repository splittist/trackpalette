#|
 This file is a part of trackpalette
|#

(asdf:defsystem trackpalette
  :version "0.0.1"
  :license "AGPL3.0"
  :author "John Q. Splittist <splittist@splittist.com>"
  :maintainer "John Q. Splittist <splittist@splittist.com>"
  :description "Convert tracked changes to styled text"
  :homepage "https://github.com/splittist/trackpalette"
  :bug-tracker "https://github.com/splittist/trackpalette/issues"
  :source-control (:git "https://github.com/splittist/trackpalette.git")
  :serial T
  :components ((:file "package")
               (:file "trackpalette"))
  :depends-on (#:alexandria
               #:serapeum
               #:uiop
               #:plump
               #:clss
               #:lquery
               #:docxplora))
