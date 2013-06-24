
; We'll do run length encoding here.
;
; (1 1 2 3 4 4 4) -> ((2 1) 2 3 (3 4))

(defun run-length-encode-rec (lst)
  (if (null lst)
      lst
      (run-length-encode-rec-helper (list (list 1 (car lst))) (cdr lst))))

; If last element is something like (1 4), this will set it to 4.
(defun fix-last-elt (lst)
  (if (null lst)
      lst
      (let ((last-elt (car (last lst))))
        (if (and (consp last-elt)
                 (eql 1 (car last-elt)))
            (setf (car (last lst)) (cadr last-elt))))))
;
;           Note that in the above line (setf last-elt xxx) will not work.

; If the last element is something like (2 a), this will set it to (3 a).
(defun inc-last-elt (lst)
  (let ((last-elt (car (last lst))))
    (setf (car last-elt) (1+ (car last-elt)))))

(defun run-length-encode-rec-helper (partial-lst remainder-lst)
  (if (null remainder-lst)
      partial-lst
      (let ((last-sym-partial (cadar (last partial-lst))))
           (if (eql (car remainder-lst) last-sym-partial)
               (progn
                     (inc-last-elt partial-lst)
                     (run-length-encode-rec-helper partial-lst (cdr remainder-lst)))
               (progn
                     (fix-last-elt partial-lst)
                     (run-length-encode-rec-helper (append partial-lst (list (list 1 (car remainder-lst)))) (cdr remainder-lst)))))))

;;;;;;;;;;;;
;;
;; Another way to do run length encoding, have three parts:
;; done, doing and todo
;;
;;;;;;;;;;;;

; Append doing to done.
(defun append-doing-done-not-null (done doing)
  (if (eql 1 (car doing))
      (append done (list (cdr doing)))
      (append done (list (list (car doing) (cdr doing))))))

(defun append-doing-done (done doing)
  (if (null done)
      (if (eql 1 (car doing))
          (list (cdr doing))
          (list (list (car doing) (cdr doing))))
      (append-doing-done-not-null done doing)))

; Run length encoding.
(defun rle-helper (done doing todo)
  (if (null todo)
      (append-doing-done done doing) 
      (if (eql (cdr doing) (car todo))
          (rle-helper done (cons (1+ (car doing)) (cdr doing)) (cdr todo))
          (rle-helper (append-doing-done done doing) (cons 1 (car todo)) (cdr todo)))))

(defun rle (lst)
  (if (or (null lst)
          (eql 1 (length lst)))
      lst
      (cdr (rle-helper nil nil lst)))) 

;
; Uncompressing a run length encoded list.
;
(defun uncompress (lst)
  (if (null lst)
      lst
      (if (not (consp (car lst)))
          (cons (car lst) (uncompress (cdr lst)))
          (append (unroll (car lst)) (uncompress (cdr lst))))))

(defun unroll (elem)
  (let ((fst (first elem))
        (snd (second elem))
        (lst nil))
       (dotimes (i fst)
                (setf lst (cons snd lst)))
       lst))
