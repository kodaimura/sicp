(import (r7rs))

;記号データ

;2.53
(define (_memq item x)
	(cond 
		((null? x) #f)
		((eq? item (car x)) x)
		(else (memq item (cdr x)))))


(print (list 'a 'b 'c'))
(print (list 'george))
(print (cdr '((x1 x2) '(y1 y2))))
(print (cadr '((x1 x2) '(y1 y2))))
(print (pair? (car '(a short list))))
(print (memq 'red '((red shoes) (blue socks))))
(print (memq 'red '(red shoes blue socks)))


;2.54
(define (_equal? x y)
	(cond
		((and (null? x) (null? y)) #t)
		((or (null? x) (null? y)) #f)
		((eq? (car x) (car y)) (equal? (cdr x) (cdr y)))
		(else #f)))

(print (equal? '(this is a list) '(this is a list)))
(print (equal? '(this is a list) '(this (is a) list)))
(print (_equal? '(this is a list) '(this is a list)))
(print (_equal? '(this is a list) '(this (is a) list)))


;記号微分
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))

;(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))

;加数
(define (addend0 s) (cadr s))

;被加数
(define (augend0 s) (caddr s))

(define (product0? x)
	(and (pair? x) (eq? (car x) '*)))

;乗数
(define (multiplier0 p) (cadr p))

;被乗数
(define (multiplicand0 p) (caddr p))


(define (=number? exp num)
	(and (number? exp) (= exp num)))


(define (make-sum0 a1 a2)
	(cond 
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))))


(define (make-product0 m1 m2)
	(cond
		((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))))


(define (deriv0 exp var)
	(cond
		((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum0 (deriv0 (addend0 exp) var)
					   (deriv0 (augend0 exp) var)))
		((product0? exp)
			(make-sum0
				(make-product0 (multiplier0 exp)
						  	   (deriv0 (multiplicand0 exp) var))
				(make-product0 (deriv0 (multiplier0 exp) var)
							   (multiplicand0 exp))))
		(else (error "unknown expression type -- DDERIV" exp))))

(print (deriv0 '(+ x 3) 'x))
(print (deriv0 '(* x y) 'x))
(print (deriv0 '(* (* x y) (+ x 3)) 'x'))


;2.56
(define (exponentiation? exp)
	(and (pair? exp) (and (eq? (car exp) '**))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation x n)
	(cond 
		((=number? n 0) 1)
		((=number? n 1) x)
		(else (list '** x n))))

;2.57
(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))

;加数
(define (addend s) (cadr s))

;被加数
(define (augend s)
	(if (null? (cdddr s))
    	(caddr s)
      	(cons '+ (cddr s))))


(define (product? x)
	(and (pair? x) (eq? (car x) '*)))

;乗数
(define (multiplier p) (cadr p))

;被乗数
(define (multiplicand p)
  	(if (null? (cdddr p))
      	(caddr p)
      	(cons '* (cddr p))))


(define (=number? exp num)
	(and (number? exp) (= exp num)))


(define (make-sum a1 a2)
	(cond 
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))))


(define (make-product m1 m2)
	(cond
		((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))))


(define (deriv exp var)
	(cond
		((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum (deriv (addend exp) var)
					  (deriv (augend exp) var)))
		((product? exp)
			(make-sum
				(make-product (multiplier exp)
						  	  (deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var)
							  (multiplicand exp))))
		((exponentiation? exp)
			(let ((e (exponent exp))
				  (b (base exp)))
				(make-product 
					(make-product
						e
						(make-exponentiation b (- e 1)))
					(deriv b var))))
		(else (error "unknown expression type -- DDERIV" exp))))

(print (deriv '(* x y (+ x 3)) 'x))
(print (deriv '(** x 3) 'x))
(print (deriv '(+ (** (+ (** x 2) 3) 5) (* x 3)) 'x))

;集合
(define (element-of-set? x set)
	(cond 
		((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))

(define (intersection-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1)
				  (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

;2.59
(define (union-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) set2)
		((element-of-set? (car set1) set2)
			(union-set (cdr set1) set2))
		(else 
			(cons (car set1)
				  (union-set (cdr set1) set2)))))

(print (intersection-set '(1 2 3) '(3 4 5)))
(print (union-set '(1 2 3) '(3 4 5)))