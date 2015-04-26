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

;; Problem 18 and 67
(defun line-to-list (line)
  (with-input-from-string (s line)
    (let ((r nil))
      (do ((line (read s nil 'eof)
                 (read s nil 'eof)))
          ((eql line 'eof))
	(push line r))
      (reverse r))))

(defun 18-make-list (&optional (file-path "./p067_triangle.txt"))
  (let ((in (open file-path :if-does-not-exist nil))
	(out '()))
    (when in
      (loop for line = (read-line in nil)
	 while line do (setf out (append out (list (line-to-list line)))))
      (close in))
    out))

(defvar *18list2* '(
    (75)
    (95 64)
    (17 47 82)
    (18 35 87 10)
    (20 04 82 47 65)
    (19 01 23 75 03 34)
    (88 02 77 73 07 63 67)
    (99 65 04 28 06 16 70 92)
    (41 41 26 56 83 40 80 70 33)
    (41 48 72 33 47 32 37 16 94 29)
    (53 71 44 65 25 43 91 52 97 51 14)
    (70 11 33 28 77 73 17 78 39 68 17 57)
    (91 71 52 38 17 14 91 43 58 50 27 29 48)
    (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
    (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)
))

(defun euler18 (&optional (lst (18-make-list)) (i (1- (length lst))) (carry (nth i lst)))
  (cond
    ((zerop i) (car carry))
    (t
     (let ((this-lst (nth (1- i) lst)) (next-carry '()))
       (loop for l from 0 to (1- (length this-lst))
	  do (let ((sum1 (+ (nth l this-lst) (nth l carry)))
		   (sum2 (+ (nth l this-lst) (nth (1+ l) carry))))
	       (if (> sum1 sum2)
		   (setf next-carry (append next-carry (list sum1)))
		   (setf next-carry (append next-carry (list sum2))))))
       (euler18 lst (1- i) next-carry)))))

;; Problem 19
(defun get-day (month day year)
  (nth-value 6 (decode-universal-time
   (encode-universal-time 0 0 0 day month year))))

(defun euler19 (&optional (year 1901) (month 1) (count 0))
  (cond
    ((= year 2001) count)
    (t (cond
       ((= month 13)
	(euler19 (1+ year) 1 count))
       (t (if (= (get-day month 1 year) 6)
	    (euler19 year (1+ month) (1+ count))
	    (euler19 year (1+ month) count)))))))

;; Problem 20
(defun factorial (n &optional (acc 1))
  (cond
    ((zerop n) acc)
    (t (factorial (1- n) (* n acc)))))

(defun euler20 (&optional (n 100))
  (reduce #'+ (loop for l across (format nil "~a" (factorial n))
       collecting (digit-char-p l))))
