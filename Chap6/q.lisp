;;;; Answers to Chapter 6 questions.

;;; 1
;;; tokens - :test and :start arguments.
(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (x) (not (funcall test x))) str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str :test test :start p2)
                    nil)))
        nil))) 

;;; 2
;;; bin-search - :key, :test, :start and :end
;(defun bin-search (seq val &key (key #'identity) (test #'<) (start 0) end)
;  (labels ((midpoint (low high) (+ low (floor (/ (- high low) 2)))))
;    (let ((mid (midpoint start end))
;          (test-val (funcall test 

;;; 3
(defun num-args (&rest args)
  (length args))

;;; 4
;;; most - highest and second highest.

(defun my-max (a1 a2 &key (test #'>) (key #'identity))
  (cond
    ((null a1) a2)
    ((null a2) a1)
    (t (if (funcall test (funcall key a1) (funcall key a2))
                   a1
                   a2))))

(defun most-modified (fn lst)
  (case (length lst)
    (0 (values nil nil))
    (1 (values (car lst) nil))
    (2 (values (first lst) (second lst) nil))
    (t (multiple-value-bind (m1 m2) (most-modified fn (list (first lst) (second lst)))
         (multiple-value-bind (m3 m4) (most-modified fn (cdr lst)) ; we do cdr and not cddr
           (values (my-max m1 m3 :key fn) (my-max m4 (my-max m1 m3 :test #'< :key fn) :key fn)))))))
