;; 1
;;
(defun quarter-turn (arr)
  (let ((dims (array-dimensions arr)))
       (if (and (eql 2 (length dims))
                (eql (first dims) (second dims)))
           (let ((dim (first dims)))
                (if (evenp dim)
                    (quarter-turn-even arr dim)
                    (quarter-turn-odd arr dim)))
           nil)))

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

(defun quarter-turn-odd (arr x)
  arr)