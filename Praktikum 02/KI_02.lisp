;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 2.1  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-elementp (e l)
    (cond 
        ((null l) nil)
        ((equal (car l) e) t)
        (t (my-elementp e (cdr l)))))

(defun simp (expr)
    (cond
        ((constantp expr) expr)
        ((atom expr) expr)
        ;;;('(+ - * /) number number)
        ((and (my-elementp (car expr) '(+ - * /)) (numberp (cadr expr)) (numberp (caddr expr))) (eval expr))
        ;;;(+ 0 x) or (- 0 x)
        ((and (or (equal (car expr) '+) (equal (car expr) '-)) (equal (cadr expr) '0)) (caddr expr))
        ;;;(+ x 0) or (- x 0)
        ((and (or (equal (car expr) '+) (equal (car expr) '-)) (equal (caddr expr) '0)) (cadr expr))
        ;;;(* 0 x) or (* x 0)
        ((and (equal (car expr) '*) (or (equal (cadr expr) '0) (equal (caddr expr) '0))) '0)
        ;;;(* 1 x)
        ((and (equal (car expr) '*) (equal (cadr expr) '1)) (caddr expr))
        ;;;(* x 1) or (/ x 1)
        ((and (or (equal (car expr) '*) (equal (car expr) '/)) (equal (caddr expr) '1)) (cadr expr))
        ;;;(- x x)
        ((and (equal (car expr) '-) (equal (cadr expr) (caddr expr))) '0)
        ;;;(/ x x)
        ((and (equal (car expr) '/) (equal (cadr expr) (caddr expr))) '1)
	;;;(sin 0)
	((and (equal (car expr) 'sin) (equal (cadr expr) '0)) '0)
	;;;(cos 0)
	((and (equal (car expr) 'cos) (equal (cadr expr) '0)) '1)
        ;;;(exp 0)
	((and (equal (car expr) 'cos) (equal (cadr expr) '0)) '1)
	;;;(log 1)
	((and (equal (car expr) 'cos) (equal (cadr expr) '0)) '0)
        ;;;could not simplify
        (t expr)))

;;;(simp '())
;;;(simp '+)
;;;(simp '(+ 2 3))
;;;(simp '(- 2 3))
;;;(simp '(* 2 3))
;;;(simp '(/ 2 3))
;;;(simp '(+ 0 x))
;;;(simp '(+ x 0))
;;;(simp '(- 0 x))
;;;(simp '(- x 0))
;;;(simp '(* 0 x))
;;;(simp '(* x 0))
;;;(simp '(* 1 x))
;;;(simp '(* x 1))
;;;(simp '(/ x 1))
;;;(simp '(/ x x))
;;;(simp '(sin 0))
;;;(simp '(cos 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 2.2  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun full-simp (expr)
    (let ((lastexpr expr)
	 (currentexpr (simp expr)))
      (cond
	((not (equal lastexpr currentexpr))
	 (full-simp currentexpr))
	((and (atom currentexpr) (equal lastexpr currentexpr))
	 currentexpr)
	((and (not (atom currentexpr)) (equal lastexpr currentexpr) (my-elementp (car currentexpr) '(+ - * /)))
	 (simp (list (car currentexpr) (full-simp (cadr currentexpr)) (full-simp (caddr currentexpr)))))
	 (t
	  currentexpr))))

;;;(full-simp '())
;;;(full-simp '(+ 2 3))
;;;(full-simp '(- 2 2))
;;;(full-simp '(+ x 0))
;;;(full-simp '(+ x (- 2 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 2.3  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-at (expr var val)
    (full-simp (sublis (list (cons var val)) (full-simp expr))))

;;;(eval-at '(* x x) 'x 2)
;;;(eval-at '(* x x) 'x 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 2.4  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nth-deriv (expr var n)
    (if (= n 0)
        (full-simp expr)
        (full-simp (deriv (nth-deriv expr var (1- n)) var))))

;;;(nth-deriv '(* x x) 'x 1)
;;;(nth-deriv '(* x x) 'x 2)
;;;(nth-deriv '(* x x) 'x 3)
;;;(nth-deriv '(* (* x x) (* x x)) 'x 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aufgabe 2.5  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun taylor-coeff (expr var val n)
  (full-simp (list '* (list '/ (eval-at (nth-deriv expr var n) var val) (! n))
		   (list '^ (list '- 'x val)) n)))

;;;(taylor-coeff '(sin x) 'x 0 5)
