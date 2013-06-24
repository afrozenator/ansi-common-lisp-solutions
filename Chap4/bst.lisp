;;;; Binary Search Tree Functions

(defstruct node 
  elem
  (left nil)
  (right nil))

;; Insert into a binary search tree.
;;
;; The return value of this is just the newly made node, so basically you'd
;; (setf x (bst-insert nil init-elem #'<))  -   to create a new tree
;; (bst-insert x new-elem #'<) -   to insert a new element
;; (print x)    -   This changes.
;;
;; Obviously if you pass in nil, we can't change that. In that case the return value is your answer.
(defun bst-insert (bst key less-fn)
  (if (null bst)
      (make-node :elem key)
      (if (funcall less-fn key (node-elem bst))
          (if (null (node-left bst))                        ; key less than root.
              (setf (node-left bst) (make-node :elem key))
              (bst-insert (node-left bst) key less-fn))
          (if (null (node-right bst))
              (setf (node-right bst) (make-node :elem key))
              (bst-insert (node-right bst) key less-fn)))))

;; This version is the one in which the return value of the function is the new bst.
;;
;; Here you'd have to do something like
;; (setf x (bst-insert-retval x 5 #'<))
;; where
;; (setf x (bst-insert-retval nil 5 #'<)) etc
(defun bst-insert-retval (bst key less-fn)
  (if (null bst)
      (make-node :elem key)
      (if (funcall less-fn key (node-elem bst))
          (make-node :elem (node-elem bst)
                     :left (bst-insert-retval (node-left bst) key less-fn)
                     :right (node-right bst))
          (make-node :elem (node-elem bst)
                     :right (bst-insert-retval (node-right bst) key less-fn)
                     :left (node-left bst)))))

;; Both the above don't prevent inserting an already existing number in the tree. 

;; Find a node in the bst.
(defun bst-find (bst key <)
  (if (null bst)
      nil
      (let ((root-node (node-elem bst)))
        (if (eql key root-node)
            bst
            (if (funcall < key root-node)
                (bst-find (node-left bst) key <)
                (bst-find (node-right bst) key <))))))


;; Minimum number in the bst
(defun bst-min (bst)
  (if (null (node-left bst))
      (node-elem bst)
      (bst-min (node-left bst))))

(defun bst-max (bst)
  (if (null (node-right bst))
      (node-elem bst)
      (bst-max (node-right bst))))

;; Depth first traversal
(defun depth-first-traversal (bst)
  (if (not (null bst))
      (progn
        (print (node-elem bst))
        (depth-first-traversal (node-left bst))
        (depth-first-traversal (node-right bst)))))

;; In order traversal
(defun in-order-traversal (bst)
  (if (not (null bst))
      (progn
        (in-order-traversal (node-left bst))
        (print (node-elem bst))
        (in-order-traversal (node-right bst)))))

;; TODO(afro): Do the remove bst functions.

;; TODO(afro): UNKNOWN FUNCTIONS |when| & |cond|
