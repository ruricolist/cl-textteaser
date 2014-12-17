;;;; package.lisp

(defpackage #:cl-textteaser
  (:use #:cl #:alexandria #:serapeum)
  (:nicknames #:textteaser)
  (:export #:summarize #:*sentence-ideal-words*))
