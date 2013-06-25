;; 1
;;
(defun quarter-turn (arr)
  (let ((dims (array-dimensions arr)))
       (if (and (eql 2 (length dims))
                (eql (first dims) (second dims)))
           (let ((dim (first dims)))
                (if (evenp dim)
                    (quarter-turn-even-new-arr arr dim)
                    (quarter-turn-odd arr dim)))
           nil)))

;; Modifies the array in place.
(defun quarter-turn-even (arr x)
  (let ((k (/ x 2)))
    (dotimes (ix k)
      (let ((i (1+ ix)))
        (dotimes (j (1- (* 2 i)))
          (let ((llj (aref arr (- k i) (+ j (- k i))))
                (tlj (aref arr (+ j (- k i)) (1- (+ k i))))
                (trj (aref arr (1- (+ k i)) (- (1- (+ k i)) j)))
                (lrj (aref arr (- (1- (+ k i)) j) (- k i))))
               (setf (aref arr (- k i) (+ j (- k i))) lrj
	                   (aref arr (+ j (- k i)) (1- (+ k i))) llj
		                 (aref arr (1- (+ k i)) (- (1- (+ k i)) j)) tlj
			               (aref arr (- (1- (+ k i)) j) (- k i)) trj)))))))

;; Makes a new array, which is rotated.
(defun quarter-turn-even-new-arr (arr x)
  (let ((k (/ x 2))
        (narr (make-array (list x x) :initial-element nil)))
    (dotimes (ix k)
      (let ((i (1+ ix)))
        (dotimes (j (1- (* 2 i)))
          (let ((llj (aref arr (- k i) (+ j (- k i))))
                (tlj (aref arr (+ j (- k i)) (1- (+ k i))))
                (trj (aref arr (1- (+ k i)) (- (1- (+ k i)) j)))
                (lrj (aref arr (- (1- (+ k i)) j) (- k i))))
               (setf (aref narr (- k i) (+ j (- k i))) lrj
	                   (aref narr (+ j (- k i)) (1- (+ k i))) llj
		                 (aref narr (1- (+ k i)) (- (1- (+ k i)) j)) tlj
			               (aref narr (- (1- (+ k i)) j) (- k i)) trj)))))
			narr))


(defun quarter-turn-odd (arr x)
  arr)
