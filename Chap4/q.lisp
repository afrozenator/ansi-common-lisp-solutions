;;; 1
;;;
;;; Rotate an array 90deg clockwise.

;; Main function
(defun quarter-turn (arr)
  (let ((dims (array-dimensions arr)))
       (if (and (eql 2 (length dims))
                (eql (first dims) (second dims)))
           (let ((dim (first dims)))
                (if (evenp dim)
                    (quarter-turn-even-new-arr arr dim)
                    (quarter-turn-odd-new-arr arr dim)))
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


(defun quarter-turn-odd-new-arr (arr x)
  (let ((k (floor (/ x 2)))
			  (narr (make-array (list x x) :initial-element nil)))
		(dotimes (ix k)
			(let ((i (1+ ix)))
			  (dotimes (j (* 2 i))
			    (let ((llj (aref arr (- k i) (+ j (- k i))))
			          (tlj (aref arr (+ j (- k i)) (+ k i)))
			          (trj (aref arr (+ k i) (- (+ k i) j)))
			          (lrj (aref arr (- (+ k i) j) (- k i))))
			         (setf (aref narr (- k i) (+ j (- k i))) lrj
				             (aref narr (+ j (- k i)) (+ k i)) llj
					           (aref narr (+ k i) (- (+ k i) j)) tlj
						         (aref narr (- (+ k i) j) (- k i)) trj)))))
		narr))

;;; 2
;;;
;;; Use reduce (equiv foldl, foldr) to define reverse and copy-list.
(defun reduce-reverse (lst)
  (reduce #'(lambda (x y) (cons y x)) lst :from-end nil :initial-value nil))

(defun reduce-copy-list (lst)
  (reduce #'(lambda (x y) (cons x y)) lst :from-end t :initial-value nil))


;;; 3
;;;
;;; Tree with data and upto three children.

(defstruct trinary-node
	elem
	(c1 nil)
	(c2 nil)
	(c3 nil))

;; There maybe some problem in this, because if I do
;; (setf x (make-trinary-node :elem 'root :c1 (make-trinary-node :elem 'rc1) :c2 (make-trinary-node :elem 'rc2) :c3 (make-trinary-node :elem 'rc3)))
;;
;; and then, (setf y (copy-trinary-node x)) and then try to
;; (setf (trinary-node-c1 (trinary-node-c1 x)) y)
;; I get
;; Program stack overflow. RESET and both x and y are corrupted :?
(defun copy-trinary-tree (ttree)
  (if (null ttree)
      nil
      (make-trinary-node :elem (trinary-node-elem ttree)
                         :c1 (copy-trinary-tree (trinary-node-c1 tree))
                         :c2 (copy-trinary-tree (trinary-node-c2 tree))
                         :c3 (copy-trinary-tree (trinary-node-c3 tree)))))

(defun object-in-trinary-tree-p (obj ttree)
  (if (null ttree)
      nil
      (if (eql obj (trinary-node-elem ttree))
          ttree
          (or (object-in-trinary-tree-p obj (trinary-node-c1 ttree))
              (object-in-trinary-tree-p obj (trinary-node-c2 ttree))
              (object-in-trinary-tree-p obj (trinary-node-c3 ttree))))))

;;; 4
;;;
;;; In order traversal of a bst
;; In order traversal was already defined in bst.lisp


;;; 5
;;;
;;; bst-adjoin, should insert the object iff there is nothing eql to it in the tree.
;;;
;;; Assuming bst.lisp has already been 'loaded'.
(defun bst-adjoin (obj bst <)
  (if (bst-find bst obj <)
      bst
      (bst-insert bst obj <)))

;;; 6
;;;
;;; a) assoc-list to hash-table
;;
;; how to make an assoc list
;; (setf a (cons (cons 'a 'b) nil))
;; (push (cons 'j 'i) a)
(defun assoc-list-to-hash-table (asclst)
  (let ((x (make-hash-table)))
       (dolist (i asclst)
         (setf (gethash (car i) x) (cdr i)))
       x))

;;; b) hash-table to assoc list
(defun hash-table-to-assoc-list (ht)
  (let ((x nil))
       (maphash #'(lambda (k v) (push (cons k v) x)) ht)
       x))