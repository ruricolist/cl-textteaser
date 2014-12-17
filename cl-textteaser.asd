;;;; cl-textteaser.asd

(asdf:defsystem #:cl-textteaser
  :serial t
  :description "Describe cl-textteaser here"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:serapeum
               #:cl-libstemmer
               #:cl-ppcre)
  :components ((:file "package")
               (:file "params")
               (:file "stop-words")
               (:file "cl-textteaser")))
