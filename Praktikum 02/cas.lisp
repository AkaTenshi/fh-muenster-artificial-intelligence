;;;
;;; Copyright (c) 2014 Michael Tuexen
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;

(defun !(n)
  "Factorial of natural number n."
  (assert (and (integerp n)
               (or (zerop n) (plusp n)))
          (n)
          "Factorial is not defined for ~S." n)
  (if (= n 0)
    1
    (* n (! (1- n)))))
;;; (! 0)
;;; (! 1)
;;; (! 10)

(defun deriv (expr var)
  "Compute the derivative of expr using var"
  (cond ((constantp expr)
         0)
        ((atom expr)
         (if (equal expr var) 1 0))
        ;;; (u + v)' = u' + v'
        ((equal (car expr) '+)
         (list '+ (deriv (cadr expr) var) (deriv (caddr expr) var)))
        ;;; (u - v)' = u' - v'
        ((equal (car expr) '-)
         (list '- (deriv (cadr expr) var) (deriv (caddr expr) var)))
        ;;; (u * v)' = u' * v + u * v'
        ((equal (car expr) '*)
         (list '+ (list '* (deriv (cadr expr) var) (caddr expr))
                  (list '* (cadr expr) (deriv (caddr expr) var))))
        ;;; (u / v)' = (u' * v - u * v') / (v * v)
        ((equal (car expr) '/)
         (list '/ (list '- (list '* (deriv (cadr expr) var) (caddr expr))
                           (list '* (cadr expr) (deriv (caddr expr) var)))
                  (list '* (caddr expr) (caddr expr))))
        ;;; sin(u)' = cos(u) * u'
        ((equal (car expr) 'sin)
         (list '* (list 'cos (cadr expr))
                  (deriv (cadr expr) var)))
        ;;; cos(u)' = 0 - sin(u) * u'
        ((equal (car expr) 'cos)
         (list '- 0 (list '* (list 'sin (cadr expr))
                             (deriv (cadr expr) var))))
        ;;; exp(u)' = exp(u) * u'
        ((equal (car expr) 'exp)
         (list '* (list 'exp (cadr expr))
                  (deriv (cadr expr) var)))
        ;;; ln(u)' = u'/u
        ((equal (car expr) 'log)
         (list '/ (deriv (cadr expr) var)
                  (cadr expr)))
       (t (list 'deriv expr var))))
;;; (deriv 1 'x)
;;; (deriv 'x 'x)
;;; (deriv 'x 'y)
;;; (deriv '(* x x) 'x)
;;; (deriv '(/ 1 x) 'x)
;;; (deriv '(cos x) 'x)
;;; (deriv '(sin (* x x)) 'x)
;;; (deriv '(sin (exp (sin (exp x)))) 'x)
;;; (deriv '(log x) 'x)
;;; (deriv '(sin (tan x)) 'x)

(defun taylor-series (expr var at n)
  (let ((coeffs '()))
    (dotimes (i n (reverse coeffs))
        (setf coeffs (cons (taylor-coeff expr var at i) coeffs)))))
;;; (taylor-series '(log x) 'x 1 10)
;;; (taylor-series '(exp x) 'x 0 10)
;;; (taylor-series '(sin x) 'x 0 10)
;;; (taylor-series '(cos x) 'x 0 10)