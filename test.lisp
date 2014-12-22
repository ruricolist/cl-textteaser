(in-package #:cl-textteaser)

(def-suite cl-textteaser)

(in-suite cl-textteaser)

(test empty-sentence-length
  (is (= 0 (weight-of-sentence-length '()))))

(test non-empty-sentence-length
  (is (= 1/4 (weight-of-sentence-length (make-list 5)))))

(test ideal-sentence-length
  (is (= 1 (weight-of-sentence-length (make-list *sentence-ideal-words*)))))

(def stop-words-sentence '("hereafter" "hereby" "herein"))

(def no-stop-words-sentence '("Accomodation" "globalization" "emancipation"))

(def title no-stop-words-sentence)

(def text-for-keywords
  (mapconcat #'identity
             (append (list "oneone")
                     (make-list 2 :initial-element "twotwo")
                     (make-list 3 :initial-element "threethree"))
             " "))

(test stop-word-title-score
  (is (zerop (title-score title stop-words-sentence))))

(test no-stop-word-title-score
  (is (= 1 (title-score title no-stop-words-sentence))))

(test keywords-descending
  (equal '(("threethree" . 3) ("twotwo" . 2) ("oneone" . 1))
         (text-keywords text-for-keywords)))
