; 7
; Takes in a list and gives out true if atleast one element in it is a list.
(defun is-list-in-list (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
          T
          (is-list-in-list (cdr lst)))))

; Here we handle the case that the list can contain nils, we don't count these
; as lists, for the sake of fun. So a list contains a list if any elements in
; the list apart from nil are lists.
(defun is-list-in-list-iterative (lst)
  (let ((found-list-in-list nil))
    (do ((x lst (cdr x)))
      ((or found-list-in-list (null (cdr x))) found-list-in-list)
      (if (and (listp (car x)) (not (null (car x))))
          (setf found-list-in-list T)))))

; 8
;
; Iterative and Recursive versions of these functions:
;
; 1. Taken in an integer and prints those many dots.
;
; 2. Takes a list and a symbol and counts the number of times the symbol
;    occurs in the list (the actual question just asks for the symbol a).

; TODO(afro): Find out how to round up/down the number to an integer.
;             And how to return void.

(defun print-dots-rec (n)
  (if (not (integerp n))
      nil
      (if (not (zerop n))  ; If the input isn't an integer, we can just check
        (progn             ; greater than zero or something here.
          (format t ".")
          (print-dots-rec (- n 1))))))

(defun print-dots-itr (n)
  (dotimes (i n)
    (format t ".")))

(defun count-symbol-rec (sym lst)
  (count-symbol-rec-helper sym lst 0))

(defun count-symbol-rec-helper (sym lst cnt)
  (if (null lst)
      cnt
      (if (eql sym (car lst))
          (count-symbol-rec-helper sym (cdr lst) (1+ cnt))
          (count-symbol-rec-helper sym (cdr lst) cnt))))

(defun count-symbol-itr (sym lst)
  (let ((cnt 0))
    (dolist (x lst)
      (if (eql sym x)
          (setf cnt (1+ cnt))))
    cnt))
      
