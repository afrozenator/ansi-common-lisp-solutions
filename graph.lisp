; Neighbours
;
; This assumes that the graph is an associative list:
; ((v1 n11 n12 n13) (v2 n21 n22 n23) ...)
(defun neighbours (node network)
  (cdr (assoc node network)))

; queue - Assoc List of node and its path from start node.
; ((n1 p31 p21 p11 s) (n2 p22 p21 s) ...)

; Add node with path to queue, if not already exists.
; This will not modify the input.
(defun add-to-queue (node path queue)
  (if (null (assoc node queue))
      (append queue (list (cons node path)))
      queue))

; Get first node.
(defun get-first-node (queue)
  (let ((node (caar queue)))
     (progn
        (format t "Node: ~A~%" node)
        node)))

; Path to first node.
(defun path-to-first-node (queue)
  (let ((node (cdar queue)))
     (progn
        (format t "Path to Node: ~A~%" node)
        node)))

; Neighbours of first node.
(defun neighbours-first-node (node net)
  (progn 
	  (format t "..node..~A--net--~A~%" node net)
	  (let ((nbrs (neighbours node net)))
	    (progn 
		    (format t "Neighbours: ~A~%" nbrs)
		    nbrs))))

; Do a breadth first search
(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((v (get-first-node queue))
            (path-to-node (path-to-first-node queue)))
            (let ((nbrs (neighbours-first-node v net)))
              (if (eql end v)
                (reverse (cons end path-to-node))
                (progn
                  (format t "node: ~A~%" v)
                  (format t "path-to-node: ~A~%" path-to-node)
                  (format t "nbrs: ~A~%" nbrs)
                  (dolist (x nbrs)
                    (setf queue (add-to-queue x (cons v path-to-node) queue)))
                  (setf queue (cdr queue))
                  (format t "queue: ~A~%" queue)
                  (bfs end queue net)))))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;
; ((a b c) (b c) (c d))
;
; (setf net '((a b c) (b c) (c d)))
; (bfs 'd '((a nil)) net)