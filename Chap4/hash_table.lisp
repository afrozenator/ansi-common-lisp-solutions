;;;; Hash Tables

;; Hash Tables are made by the make-hash-table call.
;; (setf ht (make-hash-table))
;;
;; (gethash 'colour ht)   ; returns nil nil
;;
;; first is value associated with the key,
;; the second is if there is a value under that key.
;;
;; to insert, just use setf with get-hash
;;
;; (setf (gethash 'colour ht) 'red)
;;
;; (gethash 'colour ht)
;; RED
;; T

;; push is a synonym for setf in HT, but it keeps appending stuff into the value.

;; Can insert even functions into a hash-table
;;
;; (push "doesn't take keyword arguments" (gethash #'our-member bugs))
;;

;; remove with remhash
;;
;; (remhash 'colour ht)

;; maphash, takes in a function of two values and a hash-table, function will be
;; called on each key and value in no particular order.
;;
;; (maphash #'(lambda (key val) (format t "~A = ~A ~%" key val)) ht)
;;
;;
;; (setf ht (make-hash-table :size 100))   ; reserves for 100 elements, not bytes etc
;;
;; (setf ht (make-hash-table :test #'equal))