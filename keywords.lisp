(in-package #:cl-textteaser)

(shadow 'keyword)

(defgeneric keyword-blog-count (service blog))
(defgeneric keyword-category-count (service category))
(defgeneric keyword-blog-score (service word blog))
(defgeneric keyword-category-score (service word category))
(defgeneric add-keyword (service keyword))

(defconstant +no-category+ '%no-category)

(defclass keyword ()
  ((word :initarg :word :type string :reader keyword-word)
   (count :initarg :count :type (integer 0 *) :reader keyword-count)
   (link :initarg :link :reader keyword-link)
   (blog :initarg :blog :reader keyword-blog)
   (category :initarg :category :reader keyword-category))
  (:default-initargs :category +no-category+))

(defclass dummy-keyword-service ()
  ())

(defmethod keyword-blog-count ((self dummy-keyword-service) blog)
  (declare (ignore blog))
  1)

(defmethod keyword-category-count ((self dummy-keyword-service) category)
  (declare (ignore category))
  1)


(defmethod keyword-blog-score ((self dummy-keyword-service) word blog)
  (declare (ignore word blog))
  1)

(defmethod keyword-category-score ((self dummy-keyword-service) word category)
  (declare (ignore word category))
  1)

(defmethod add-keyword ((self dummy-keyword-service) keyword)
  (declare (ignore keyword)))

(defclass trivial-keyword-service ()
  ((keywords :initform nil :accessor keywords-of)))

(defmethod keyword-blog-count ((self trivial-keyword-service)
                               (blog string))
  (count blog
         (keywords-of self)
         :test #'equal
         :key #'keyword-blog))

(defmethod keyword-category-count ((self trivial-keyword-service)
                                   (category string))
  (count category
         (keywords-of self)
         :test #'equal
         :key #'keyword-category))

(defmethod keyword-blog-score ((self trivial-keyword-service)
                               (word string)
                               (blog string))
  (count-if (lambda (keyword)
              (and (equal (keyword-word keyword) word)
                   (equal (keyword-blog keyword) blog)))
            (keywords-of self)))

(defmethod keyword-category-score ((self trivial-keyword-service)
                                   (word string)
                                   (category string))
  (count-if (lambda (keyword)
              (and (equal (keyword-category keyword) category)
                   (equal (keyword-word keyword) word)))
            (keywords-of self)))

(defmethod add-keyword ((self trivial-keyword-service) (keyword keyword))
  (push keyword (keywords-of self)))
