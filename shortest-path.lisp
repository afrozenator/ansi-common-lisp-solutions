(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((vn (car queue)) (vertex (car vn)) (path (cdr vn)))
        (if (eql vertex end)
            (reverse (cons end path))
            (bfs end (updated-queue queue net) net)))))

; removes the first element and adds its neighbours at the end
; also updates their path
(defun updated-queue queue net
  (let ((x (pop queue)) (neighbours (assoc (car x) net)) (path (cdr x)))
    (dolist (y neighbours)
      (append queue (list y (cons (car x) path))))
       queue))
     
