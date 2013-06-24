; Binary Search
;
; Binary Search an object in a given sorted vector. We assume ascending sort.

(defun binary-search (obj vec)
  (binary-search-helper 0 (1- (length vec)) obj vec))

(defun binary-search-helper (low high obj vec)
  (if (< high low)
      nil
      (let ((mid (+ low (floor (/ (- high low) 2)))))
        (let ((elem-mid (svref vec mid)))
             (if (eql elem-mid obj)
                 obj
                 (if (> obj elem-mid)
                     (binary-search-helper (1+ mid) high obj vec)
                     (binary-search-helper low (1- mid) obj vec)))))))

;;;
;;; Splitting a string into tokens, what a token is is given by the test, test.
;;;
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if (function (lambda (c) (not (funcall test c))))
                               str :start p1)))
             (cons (subseq str p1 p2)
                   (if p2
                       (tokens str test p2)
                       nil)))
        nil)))

;; We want all printable characters, but not a space.
(defun non-separator (c)
  (and (not (char= #\  c))
       (graphic-char-p c)))

;; Parse dates of the form 15 Aug 2013, i.e. dd mon yyyy
(defun parse-date (date)
  (let ((toks (tokens date #'non-separator 0)))
    (if (eql 3 (length toks))
        (list (parse-integer (first toks))
              (parse-month (second toks))
              (parse-integer (third toks)))
        (print "Improper format"))))

;; Array of month names.
(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

;; Parse month
(defun parse-month (mon)
  (let ((p (position mon month-names :test (function string-equal))))
    (if p
        (1+ p)
        p)))

;; Our code to parse integers.
(defun my-parse-integer (str)
  (if (not (every #'digit-char-p str))
      nil
      (let ((acc 0))
           (dotimes (pos (length str))
                   (setf acc (+ (digit-char-p (char str pos)) (* 10 acc))))
           acc)))
