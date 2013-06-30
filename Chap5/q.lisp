
;;; 1
;; a)
;; (let ((x (car y)))
;;   (cons x x))
;((lambda (x)
;  (cons x x))
; (car y))

;; b)
;; (let* ((w (car x))
;;        (y (+ w z)))
;;       (cons w y))
;; TODO(afroz): Do via hand.

;;; 2
(defun mystery (x y)
  (cond
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery x (cdr y))))
            (and z (1+ z))))))

;;; 3
;; Ideally we should have imported the list to hash-map function
;; from Chap 4.
(defun my-lazy-square (x)
  (case x
    (0 0)
    (1 1)
    (2 4)
    (3 9)
    (4 16)
    (5 25)
    (otherwise (* x x))))

;;; 4
;; Couldn't find num-month, even assuming it is month-num, will do after
;; implementing the whole example myself.

;;; 5
;; In the text it seems that a set is being returned.
(defun precedes-itr (x v)
  (let ((prc nil))
       (dotimes (i (1- (length v)) prc)
               (if (eql x (aref v (1+ i)))
                   (push (aref v i) prc)))))

(defun precedes-rec (x v)
  (let ((prc nil))
       (precedes-rec-helper x v prc 0)))

(defun precedes-rec-helper (x v prc num)
   (if (>= num (1- (length v)))
       prc
       (progn (if (eql x (aref v (1+ num)))
                  (push (aref v num) prc))
              (precedes-rec-helper x v prc (1+ num)))))

;;; 6
(defun intersperse (o l)
  (cdr (reduce #'(lambda (elt lst) (cons o (cons elt lst))) l :from-end t :initial-value nil)))

;; This version has a little problem, since this is becomming a dotted list,
;; the last cdr isn't nil it is the last element in the original list.
(defun intersperse-2 (o l)
  (reduce #'(lambda (elt lst) (cons elt (cons o lst))) l :from-end t))

;;; 7
;;;
;;; Program to find if successive difference is 1.

;; Returns true if |x - y| = 1. For stricter versions, we can replace this
;; with x - 1 = y or y - 1 = x.
(defun diff-by-1-helper (x y)
  (eql 1 (abs (- x y))))

(defun diff-by-1-rec (lst)
  (if (< (length lst) 2)
      t
      (and (diff-by-1-helper (first lst) (second lst))
           (diff-by-1-rec (cddr lst)))))

(defun diff-by-1-do (lst)
  (do ((i 0 (1+ i)))
      ((eql i (1- (length lst))) t)
      (if (not (diff-by-1-helper (nth i lst) (nth (1+ i) lst)))
          (return nil))))

(defun diff-by-1-mapc-return (lst)
  (mapc #'(lambda (x y)
                  (if (not (diff-by-1-helper x y))
                      (return-from diff-by-1-mapc-return nil)))
        lst
        (cdr lst))
  t)

;;; 8
(defun max-min (lst)
  (if (eql 1 (length lst))
      (values (car lst) (car lst))
      (let ((minimum-so-far (car lst))
            (maximum-so-far (car lst)))
           (multiple-value-bind (x y) (max-min (cdr lst))
             (values (min minimum-so-far x) (max maximum-so-far y))))))

(defun max-min-t2 (lst)
  (case (length lst)
    (0 nil)
    (1 (let ((x (car lst))) (values x x)))
    (otherwise (let* ((x (car lst))
                      (max-sf x)
                      (min-sf x))
                      (multiple-value-bind (min-rest max-rest) (max-min-t2 (cdr lst))
                        (values (min min-sf min-rest) (max max-sf max-rest)))))))

;;; 9
;; TODO(afro): Do this when in mood.
