;; Problem 16
(defun euler16 (&optional (n 2) (exps 1000))
  (reduce #'+ (loop for c across (write-to-string(expt n exps))
       collecting (digit-char-p c))))

;; Problem 17
(defun num-to-string-one-digit (n)
  (let ((d1 (digit-char-p (char (write-to-string n) 0))))
    (cond
      ((= d1 1) "one")   ((= d1 2) "two")
      ((= d1 3) "three") ((= d1 4) "four")
      ((= d1 5) "five")  ((= d1 6) "six")
      ((= d1 7) "seven") ((= d1 8) "eight")
      ((= d1 9) "nine")       
      (t
       (format nil "ERROR: ~A is not a valid one digit number.~%" n)))))

(defun num-to-string-two-digit (n)
  (let ((d1 (digit-char-p (char (write-to-string n) 0)))
	(d2 (digit-char-p (char (write-to-string n) 1))))
    (cond
      ((= d1 1)
       (cond
	 ((= d2 0) "ten")       ((= d2 1) "eleven")    ((= d2 2) "twelve")
	 ((= d2 3) "thirteen")  ((= d2 4) "fourteen")  ((= d2 5) "fifteen")
	 ((= d2 6) "sixteen")   ((= d2 7) "seventeen") ((= d2 8) "eighteen")
	 ((= d2 9) "nineteen")
	 (t
	  (format nil "ERROR: ~A is not a valid two digit number.~%" n))))
      ((and (> d1 1) (< d1 10))
       (cond
	 ((= d1 2) (if (zerop d2) "twenty"  (format nil "twenty ~A"  (num-to-string d2))))
	 ((= d1 3) (if (zerop d2) "thirty"  (format nil "thirty ~A"  (num-to-string d2))))
	 ((= d1 4) (if (zerop d2) "forty"   (format nil "forty ~A"   (num-to-string d2))))
	 ((= d1 5) (if (zerop d2) "fifty"   (format nil "fifty ~A"   (num-to-string d2))))
	 ((= d1 6) (if (zerop d2) "sixty"   (format nil "sixty ~A"   (num-to-string d2))))
	 ((= d1 7) (if (zerop d2) "seventy" (format nil "seventy ~A" (num-to-string d2))))
	 ((= d1 8) (if (zerop d2) "eighty"  (format nil "eighty ~A"  (num-to-string d2))))
	 ((= d1 9) (if (zerop d2) "ninety"  (format nil "ninety ~A"  (num-to-string d2))))
	 (t
	  (format nil "ERROR: ~A is not a valid two digit number.~%" n))))
      (t
       (format nil "ERROR: ~A I don't even know what that is.~%" n)))))

(defun num-to-string-three-digit (n)
  (let ((d1    (digit-char-p  (char   (write-to-string n) 0)))
	(d2    (digit-char-p  (char   (write-to-string n) 1)))
	(d3    (digit-char-p  (char   (write-to-string n) 2)))
	(d-2-3 (parse-integer (subseq (write-to-string n) 1 3))))
    (if (zerop d2)
	(if (zerop d3)
	    (format nil "~A hundred" (num-to-string d1))
	    (format nil "~A hundred and ~A" (num-to-string d1) (num-to-string d3)))
      (format nil "~A hundred and ~A" (num-to-string d1) (num-to-string d-2-3)))))

(defun num-to-string (n)
  (let ((digits (length (write-to-string n))))
    (cond
      ((= digits 1) (num-to-string-one-digit n))
      ((= digits 2) (num-to-string-two-digit n))
      ((= digits 3) (num-to-string-three-digit n))
      ((= digits 4) "one thousand")
      (t
       (format nil "ERROR: ~A is not a valid number of digits.~%" n)))))

(defun euler17 ()
  (reduce #'+ (mapcar (lambda (s) (length (remove #\  s)))
		  (loop for n from 1 to 1000 collecting (num-to-string n)))))

;; Lol should have just done this
(defun euler17-2 ()
  (reduce #'+ (mapcar (lambda (str) (length str))
		      (loop for n from 1 to 1000 collecting
       (remove #\ (remove #\- (format nil "~R" n)))))))

;; Problem 18
