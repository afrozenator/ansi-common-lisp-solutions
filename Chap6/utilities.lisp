;;;; My own versions of
;;;;
;;;; single?
;;;; append1
;;;; map-int
;;;; filter
;;;; most

(defun single? (x)
  (and (consp x) (null (cdr x))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (f n)
  (let ((accum nil))
       (dotimes (i n)
                (push (funcall f i) accum))
       (nreverse accum)))

;;; Range of n numbers.
(defun range (n)
  (map-int #'identity n))

;;; n random numbers between a and b, excluding b.
(defun rand-list (n a b)
  (let ((l (min a b))
        (u (max a b)))
       (map-int #'(lambda (x) (+ l (random (- u l)))) n)))

;;; Just take a predicate function and filter based on that.
(defun filter (f lst)
  (let ((accum nil))
       (dolist (elm lst)
               (if (not (null (funcall f elm)))
                   (push elm accum)))
       (nreverse accum)))

;;; Take a function and add all non-null values in a list.
(defun filter-val (f lst)
  (let ((accum nil))
       (dolist (elm lst)
         (let ((val (funcall f elm)))
              (if (not (null val))
                  (push val accum))))
       (nreverse accum)))

(defun most (f lst)
  (most-least-helper f lst #'>))

(defun least (f lst)
  (most-least-helper f lst #'<))

(defun most-least-helper (f lst gt-lt)
  (if (null lst)
      nil
      (let ((m (car lst)))
           (dolist (x (cdr lst))
                   (if (funcall gt-lt (funcall f x) (funcall f m))
                       (setf m x)))
           m)))

;;;; compose, disjoin, conjoin, curry, rcurry, always

;;; compose - takes a list of arguments, and returns a function which will
;;;           apply those functions in the order given
(defun compose (&rest fns)
  (case (length fns)
    (0 #'identity)  ; Shouldn't call it with an empty argument list.
    (1 (car fns))
    (t #'(lambda (x)
           (funcall (car fns) (funcall (apply #'compose (cdr fns)) x))))))

;; As implemented in the text.
(defun compose2 (&rest fns)
  #'(lambda (x)
      (reduce #'funcall fns :from-end t :initial-value x)))


;;; disjoin - takes in predicates and returns a predicate that returns true if any
;;;           of the predicates returns true.
(defun disjoin (&rest preds)
  (case (length preds)
    (0 t)
    (1 (car preds))
    (t #'(lambda (x)
           (or (funcall (car preds) x) (funcall (apply #'disjoin (cdr preds)) x))))))

;; Another way using helper function and reduce.
(defun disjoin2-helper (pred1 pred2)
  #'(lambda (x) (or (funcall pred1 x) (funcall pred2 x))))

(defun disjoin2 (&rest preds)
  (reduce #'disjoin2-helper preds))

;; Rewriting of the above with |labels|.
(defun disjoin22 (&rest preds)
  (labels ((disjoin22-helper (pred1 pred2)
             #'(lambda (x) (or (funcall pred1 x) (funcall pred2 x)))))
    (reduce #'disjoin22-helper preds)))


;;; conjoin - just replace the above |or|s with |and|s.

;;; curry - take a function and some of its arguments, and return another function which is
;;;         just the same function but which just takes the rest of the arguments.
(defun curry (fn &rest args)
  #'(lambda (&rest x) (apply fn (append args x)))) 

(defun rcurry (fn &rest args)
  #'(lambda (&rest x) (apply fn (append x args))))

;;; always - lisp's constantly.
;;;        - Takes an argument and returns a function that returns that argument.
(defun always (arg)
  #'(lambda (&rest x) arg))
