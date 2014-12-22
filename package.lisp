;;;; package.lisp

(defpackage #:cl-textteaser
  (:use #:cl #:alexandria #:serapeum #:5am)
  (:nicknames #:textteaser)
  (:export #:summarize #:*sentence-ideal-words*))
