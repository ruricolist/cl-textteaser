;;;; cl-textteaser.lisp

(in-package #:cl-textteaser)

;;; "cl-textteaser" goes here. Hacks and glory await!

(def summary-size 5)
(def keywords-size 10)

(defun summarize (text &key (title "")
                            (summary-size *summary-size*)
                            (keywords-size *keywords-size*)
                            ((:sentence-ideal-words *sentence-ideal-words*) *sentence-ideal-words*)
                            ((:min-word-length *min-word-length*) *min-word-length*)
                            ((:default-summary-size *summary-size*) *summary-size*)
                            ((:default-keywords-size *keywords-size*) *keywords-size*)
                            (sentence-splitter #'sentences))
  (let* ((sentences (funcall sentence-splitter text))
         (title-words (words-dc title))
         (keywords (text-keywords text :limit keywords-size))
         (scores (compute-scores sentences title-words keywords))
         (best (bestn summary-size scores #'> :key #'cdr)))
    (values (mapcar #'car best)
            (mapcar #'cdr best))))

(defun sentences (text)
  (collecting
    (loop for line in (lines text)
          for sentences = (ppcre:split "(?s)(?:$|[.!?]($|[]\"'”)}]*\\s+))" line)
          do (loop for sentence in sentences
                   when (some #'alpha-char-p sentence)
                     do (collect sentence)))))

(defun words-dc (string)
  (words (string-downcase string)))

(defun keyword-score (word keywords)
  (or (assocdr word keywords :test #'equal) 0))

(defun sbs (words keywords)
  "Summation Based Selection."
  (if (no words) 0
      (loop for len from 0
            for word in words
            for score = (keyword-score word keywords)
            sum score into scores
            finally (return
                      (if (zerop scores) 0
                          (/ (* len scores)))))))

(defun dbs (words keywords)
  "Density Based Selection."
  (if (no words) 0
      (let* ((res (loop for word in words
                        for i from 0
                        for score = (keyword-score word keywords)
                        when (> score 0)
                          collect (cons score i)))
             (sum (loop for (score1 . i) in res
                        for (score2 . j) in (rest res)
                        sum (/ (* score1 score2)
                               (expt (- i j) 2))))
             (k (1+ (intersection-size words keywords :test (op (eql _ (car _)))))))
        (* (/ (* k (1+ k))) sum))))

(defun text-keywords (text &key limit)
  "An alist of words (less stop words) in TEXT and their frequencies."
  (let* ((words (words-dc text))
         (len (length words))
         (words (delete-if #'stop-word? words))
         (frequencies (hash-table-alist (frequencies words :test #'equal)))
         (test #'>) (key #'cdr)
         (keywords (if limit
                       (bestn limit frequencies test :key key)
                       (sort frequencies test :key key))))
    (values keywords len)))

(defun weight-of-sentence-position (pos sentence-count)
  (let ((n (/ pos sentence-count)))
    (case-using #'<= n
      (0 0)
      (0.1 0.17)
      (0.2 0.23)
      (0.3 0.14)
      (0.4 0.08)
      (0.5 0.05)
      (0.6 0.04)
      (0.7 0.06)
      (0.8 0.04)
      (0.9 0.04)
      (1 0.15)
      (t 0))))

(defun compute-score (sentence index title-words keywords sentence-count)
  (let* ((words (words-dc sentence))
         (title-weight (title-score title-words words))
         (length-weight (weight-of-sentence-length words))
         (position-weight (weight-of-sentence-position index sentence-count))
         (sbs (sbs words keywords))
         (dbs (dbs words keywords))
         (keyword-frequency (* 10 (/ (+ sbs dbs) 2)))
         (score (/ (+ (* 1.5 title-weight)
                      (* 2.0 keyword-frequency)
                      (* 0.5 length-weight)
                      (* 1.0 position-weight))
                   4.0)))
    (cons sentence score)))

(defun compute-scores (sentences title-words keywords)
  (let ((sentence-count (length sentences)))
    (loop for i from 0
          for s in sentences
          collect (compute-score s i title-words keywords sentence-count))))

(defun weight-of-sentence-length (words &aux (ideal *sentence-ideal-words*))
  (/ (- ideal (abs (- ideal (length words)))) ideal))

(defun title-score (title-words words)
  (let ((title-len (length title-words)))
    (if (zerop title-len)
        0
        (/ (intersection-size (sans-stop-words words) title-words)
           title-len))))

(defun intersection-size (list1 list2 &rest args)
  (length (apply #'intersection list1 list2 :test #'equal args)))
