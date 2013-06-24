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
  arr)

(defun quarter-turn-odd (arr x)
  arr)