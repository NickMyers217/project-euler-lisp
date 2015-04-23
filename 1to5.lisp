;;;; Projec Euler Answers in Common Lisp - Nick Myers

;; Problem 1
(defun euler1 (&optional (i 999) (sum 0))
  (cond
    ((= i 2) sum)
    ((or (= 0 (mod i 3)) (= 0 (mod i 5)))
     (euler1 (1- i) (+ i sum)))
    (t
     (euler1 (1- i) sum))))
;(print '===> )(princ (euler1))

;; Problem 2
(defun euler2 (cap &optional (a 1) (b 1) (sum 0))
  (let ((c (+ a b)))
    (cond
      ((> c cap) sum)
      ((= 0 (mod c 2))
       (euler2 cap b c (+ sum c)))
      (t
       (euler2 cap b c sum)))))
;(print '===> )(princ (euler2 4000000))

;; Problem 3
(defun euler3 (&optional (num 600851475143))
  (when (> num 1) 
    (do ((x 2 (1+ x))) 
	((zerop (mod num x)) 
	 (cons x (euler3 (/ num x)))))))
;(print '===> )(princ (euler3))

;; Problem 4
(defun length-n (n)
  (length (write-to-string n)))

(defun euler4 (&optional (n1 999) (n2 999) (c 3) (res 0))
  (cond
    ((and (< (length-n n1) c) (< (length-n n2) c))
     res)
    ((< (length-n n1) c)
     (euler4 (1- n2) (1- n2) c res))
    ((and
      (string= (write-to-string (* n1 n2))
	       (reverse (write-to-string(* n1 n2))))
      (> (* n1 n2) res))
     (euler4 (1- n1) n2 c (* n1 n2)))
    (t
     (euler4 (1- n1) n2 c res))))

;(print '===> )(princ (euler4))

;; Problem 5
(defun euler5 (&optional (cap 10))
  (apply #'lcm (loop for n from 1 to cap collecting n)))
;(print '===> )(princ (euler5))
