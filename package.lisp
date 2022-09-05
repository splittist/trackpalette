#|
 This file is part of trackpalette
|#

(in-package #:cl)

(defpackage #:com.splittist.trackpalette
  (:use #:cl #:docxplora)
  (:local-nicknames)
  (:export #:handle-deleted-field-codes
           #:handle-inserted-math-characters
           #:handle-deleted-math-characters
           #:handle-insertions
           #:handle-deletions
           #:handle-move-tos
           #:handle-move-froms
           #:accept-other-changes))
