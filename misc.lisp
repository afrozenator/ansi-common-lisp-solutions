; Factorial program
;
; Recursive version.
(defun factorial-recursive (n)
  (factorial-recursive-helper 1 n))

(defun factorial-recursive-helper (acc n)
  (if (zerop n)
      acc
      (factorial-recursive-helper (* acc n) (- n 1))))
;
;
; Iterative version.
(defun factorial-iterative (n)
  (do ((itr 1) (res 1))
      ((or (eql itr (1+ n)) (zerop n)) res)
      (progn
        (setf res (* res itr))
        (setf itr (1+ itr)))))
