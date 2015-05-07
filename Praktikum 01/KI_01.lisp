;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.1  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-elementp (e l)
  (cond ((null l) nil)
      ((equal (car l) e) t)
      (t (my-elementp e (cdr l)))))

;;;(my-elementp 1 '(1))
;;;(my-elementp 1 '(1 2 3))
;;;(my-elementp 1 '(2 3 4))
;;;(my-elementp 1 '(3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.2  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-subsetp (l1 l2)
  (cond ((null l1) t)
	((my-elementp (car l1) l2) (my-subsetp (cdr l1) l2))
	(t nil)))

;;;(my-subsetp '() '())
;;;(my-subsetp '() '(1))
;;;(my-subsetp '(1) '())
;;;(my-subsetp '(1) '(1))
;;;(my-subsetp '(1) '(1 2 3))
;;;(my-subsetp '(1 2) '())
;;;(my-subsetp '(1 2) '(1))
;;;(my-subsetp '(1 2) '(1 2))
;;;(my-subsetp '(1 2 3) '(4 5 6))
;;;(my-subsetp '(1 2) '(2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.3  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-adjoin (e l)
  (cond ((my-elementp e l) l)
	(t (cons e l))))

;;;(my-adjoin 1 '())
;;;(my-adjoin 1 '(1))
;;;(my-adjoin 1 '(1 2))
;;;(my-adjoin 1 '(1 2 3))
;;;(my-adjoin 1 '(2 3))
;;;(my-adjoin nil '(1 2 3))
;;;(my-adjoin t '(1 2 3))
;;;(my-adjoin 'ASDF '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.4  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-union (l1 l2)
  (cond ((null l1) l2)
	((null l2) l1)
	(t (my-union (cdr l1) (my-adjoin (car l1) l2)))))

;;;(my-union '() '())
;;;(my-union '(1) '())
;;;(my-union '() '(1))
;;;(my-union '(1) '(1))
;;;(my-union '(1) '(2))
;;;(my-union '(1) '(1 2))
;;;(my-union '(1 2) '(1))
;;;(my-union '(1 2 3) '(4 5 6))
;;;(my-union '(1 2 3) '(3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.5  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-intersection (l1 l2)
  (cond ((null l1) nil)
	((my-elementp (car l1) l2) (my-adjoin (car l1) (my-intersection (cdr l1) l2)))
	(t (my-intersection (cdr l1) l2))))

;;;(my-intersection '() '())
;;;(my-intersection '(1) '())
;;;(my-intersection '() '(1))
;;;(my-intersection '(1) '(1))
;;;(my-intersection '(1) '(1 2))
;;;(my-intersection '(1 2) '(1))
;;;(my-intersection '(1 2) '(1 2))
;;;(my-intersection '(1 2 3) '(4 5 6))
;;;(my-intersection '(1 2 3) '(2 3 4 5))
;;;(my-intersection '(1 2 3) '(1 3 5 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.6  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-set-difference (l1 l2)
  (cond ((null l2) l1)
	((null l1) nil)
	((my-elementp (car l1) l2) (my-set-difference (cdr l1) l2))
	(t (my-adjoin (car l1) (my-set-difference (cdr l1) l2)))))

;;;(my-set-difference '() '())
;;;(my-set-difference '(1) '())
;;;(my-set-difference '() '(1))
;;;(my-set-difference '(1) '(1))
;;;(my-set-difference '(1) '(1 2))
;;;(my-set-difference '(1 2) '(1))
;;;(my-set-difference '(1 2 3) '(2 3 4 5))
;;;(my-set-difference '(1 2 3) '(4 5 6))
;;;(my-set-difference '(1 2 3 4 5) '(1 3 5 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.7  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-powerset (l)
  (cond ((null l) (list nil))
	((null (cdr l)) (list nil l))
	(t (my-union (my-joins (car l) (my-powerset (cdr l))) (my-powerset (cdr l))))))

;;;(my-powerset '())
;;;(my-powerset '(1))
;;;(my-powerset '(1 2))
;;;(my-powerset '(1 2 3))

(defun my-joins (e l)
  (cond
    ((null l) nil)
    ((equal e (car l)) (my-joins e (cdr l)))
    (t (my-adjoin (my-adjoin e (car l)) (my-joins e (cdr l))))))

;;;(my-joins 1 '())
;;;(my-joins 1 '(1))
;;;(my-joins 1 '(1 2))
;;;(my-joins 1 '(1 2 3))
;;;(my-joins 1 '((1) (2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.8  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collatz (n)
  (cond ((evenp n) (/ n 2))
	(t (1+ (* 3 n)))))

;;;(collatz 0)
;;;(collatz 1)
;;;(collatz 2)
;;;(collatz 3)
;;;(collatz 4)
;;;(collatz 5)
;;;(collatz -1)
;;;(collatz -2)
;;;(collatz -3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.9  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iter (f x n)
    (if (= n 0)
	(list x)
	(cons x (iter f (funcall f x) (1- n))))))

;;;(iter #'collatz 5 5)
;;;(iter #'collatz 10 6)
;;;(iter #'collatz 13 8)
;;;(iter #'collatz 15 17)
;;;(iter #'collatz 101 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.10 ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mat-add (m1 m2)
  (assert (and (= (array-rank m1) (array-rank m2) 2) 
                 (= (array-dimension m1 0) 
                    (array-dimension m2 0))
		 (= (array-dimension m1 1)
		    (array-dimension m2 1)))
            (m1 m2) 
            "Cannot add ~S by ~S." m1 m2)
  (mat-add-1 m1 m2))
(defun mat-add-1 (m1 m2)
  (let ((size-0 (array-dimension m1 0))
	(size-1 (array-dimension m1 1)))
    (dotimes (row size-0 m1)
      (dotimes (col size-1)
	(setf (aref m1 row col) (+ (aref m1 row col) (aref m2 row col)))))))

;;;(mat-add (make-array '(2 5) :element-type t :initial-element 1) (make-array '(2 3) :element-type t :initial-element 2))
;;;(mat-add (make-array '(2 2) :element-type t :initial-contents '((1 1) (1 1))) (make-array '(2 2) :element-type t :initial-contents '((3 3) (2 2))))
;;;(mat-add (make-array '(2 3) :element-type t :initial-contents '((1 1 1) (1 1 1))) (make-array '(2 3) :element-type t :initial-contents '((3 3 3) (2 2 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.11 ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mat-mul (m1 m2)
  (assert (and (= (array-rank m1) (array-rank m2) 2) 
                 (= (array-dimension m1 1) 
                    (array-dimension m2 0))) 
            (m1 m2) 
            "Cannot multiply ~S by ~S." m1 m2)
  (mat-mul-1 m1 m2))
(defun mat-mul-1 (m1 m2)
  (let ((m1-row (array-dimension m1 0))
	(m1-col (array-dimension m1 1))
	(m2-col (array-dimension m2 1)))
    (let ((mr (make-array (cons m1-row (cons m2-col '())) :element-type t :initial-element 0)))
    (dotimes (i m1-row mr)
      (dotimes (j m2-col)
	(dotimes (k m1-col)
	  (incf (aref mr i j) (* (aref m1 i k) (aref m2 k j)))))))))

;;;(mat-mul (make-array '(2 2) :element-type t :initial-element 6) (make-array '(3 3) :element-type t :initial-element 5))
;;;(mat-mul (make-array '(2 2) :element-type t :initial-contents '((1 1) (1 1))) (make-array '(2 2) :element-type t :initial-contents '((3 3) (2 2))))
;;;(mat-mul (make-array '(2 2) :element-type t :initial-contents '((1 2) (3 4))) (make-array '(2 2) :element-type t :initial-contents '((8 7) (6 5))))
;;;(mat-mul (make-array '(5 5) :element-type t :initial-element 6) (make-array '(5 5) :element-type t :initial-element 5))
;;;(mat-mul (make-array '(3 2) :element-type t :initial-contents '((1 1) (1 1) (1 1))) (make-array '(2 3) :element-type t :initial-contents '((3 3 3) (2 2 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 1.12 ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mat-pow (m n)
  (assert (and (= (array-rank m) 2)
                 (= (array-dimension m 1) 
                    (array-dimension m 0))
		 (>= n 0)) 
            (m n) 
            "Cannot raise ~S to the power of ~S." m n)
  (mat-pow-1 m n))
(defun mat-pow-1 (m n)
  (cond ((= n 1) m)
	((evenp n) (mat-pow-1 (mat-mul m m) (/ n 2)))
	((oddp n) (mat-mul m (mat-pow-1 m (1- n))))))

;;;(mat-pow (make-array '(2 2) :element-type t :initial-element 1) -1)
;;;(mat-pow (make-array '(2 3) :element-type t :initial-element 1) 2)
;;;(mat-pow (make-array '(2 2) :element-type t :initial-element 1) 0)
;;;(mat-pow (make-array '(2 2) :element-type t :initial-element 1) 3)
;;;(mat-pow (make-array '(3 3) :element-type t :initial-contents '((1 2 3) (3 2 1) (2 3 1))) 3)
;;;(mat-pow (make-array '(2 2) :element-type t :initial-contents '((4 5) (8 9))) 3)
