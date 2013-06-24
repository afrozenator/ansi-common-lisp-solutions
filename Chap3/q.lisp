; 2
;
; Order preserving union?
; I didn't understand the question, according to the example, it isn't
; clear what is required. It preserves the order in the first argument
; but not in the second.


; 3
;
; Occurrences.
(defun occurrences (lst)
  (sort (symbol-and-counts lst) #'> :key #'cdr))

; This gives a list of pairs, (symbol . count)
(defun symbol-and-counts (lst)
  (let ((x nil))
    (dolist (y lst)
            (let ((z (member y x :key #'car :test #'eql)))
                 (if (null z)
                     (setf x (cons (cons y 1) x))
                     (setf (cdar z) (1+ (cdar z))))))
    x))

; 4
;
; Since they are list, they are conses, and then it will do pointer equality
; and they are different conses, although with the same atom.


; 5
;
; pos+

;  mapcar
(defun pos+ (lst)
  (let ((x nil)
        (l (length lst)))
       (dotimes (i l)
                (setf x (cons i x)))
       (setf x (reverse x))
       (mapcar #'+ x lst)))

; recursion
(defun pos+-rec (lst)
  (reverse (pos+-rec-helper nil 0 lst)))

(defun pos+-rec-helper (acc pos lst)
  (if (null lst)
      acc
      (pos+-rec-helper (cons (+ pos (car lst)) acc) (1+ pos) (cdr lst))))

; iteration
(defun pos+-iter (lst)
  (do ((pos 0 (1+ pos))
       (l lst (cdr l))
       (x   nil))
      ((null l) (reverse x))
      (setf x (cons (+ pos (car l)) x))))

; 6
;
; I don't see the point in this question. I'll do them with the usual car, cdr
; semantics. Interchanging car & cdr should do?

; a & b
; How do you even do these? 

; c & d
; These are easy.


; 7
;
; TODO(afro): Later.

; 8
;
; Show dots function.
(defun showdots (lst)
  (if (not (null lst))
      (progn
        (format t "(")
        (if (consp (car lst))
            (showdots (car lst))
            (format t "~A" (car lst)))
        (format t " . ")
        (showdots (cdr lst))
        (format t ")"))
       (format t "NIL")))
       
; 9
;
; Longest finite path in a network, that contains cycles.
;
; TODO(afro): Later.
