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

;順序づけられたリストとしての集合
(define (element-of-set2? x set)
	(cond 
		((null? set) #f)
		((= x (car set)) #t)
		((< x (car set)) #f)
		(else (element-of-set2? x (cdr set)))))


;2.61
(define (adjoin-set2 x set)
	(cond
		((null? set) (list x))
		((= x (car set)) set)
		((< x (car set)) (cons x set))
		(else (cons (car set) (adjoin-set2 x (cdr set))))))

(define set0 '(3 7 9))
(print (adjoin-set2 2 set0))
(print (adjoin-set2 6 set0))
(print (adjoin-set2 9 set0))


;2.62
(define (union-set2 set1 set2)
	(cond
		((null? set1) set2)
		((null? set2) set1)
		(else
			(let ((x1 (car set1))
			      (x2 (car set2)))
				(cond 
					((= x1 x2) (cons x1 (union-set2 (cdr set1) (cdr set2))))
					((< x1 x2) (cons x1 (union-set2 (cdr set1) set2)))
					(else (cons x2 (union-set2 set1 (cdr set2)))))))))

(print (union-set2 '(1 3 6) '(2 4 5)))
(print (union-set2 '(1 3 6) '(4 5)))
(print (union-set2 '(1 3 6) '(4 5 6)))


;二進木としての集合
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
	(list entry left right))

(define (element-of-set3? x set)
	(cond
		((null? set) #f)
		((= x (entry set)) #t)
		((< x (entry set))
			(element-of-set3? x (left-branch set)))
		((> x (entry set))
			(element-of-set3? x (right-branch set)))))

(define (adjoin-set3 x set)
	(cond
		((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
			(make-tree (entry set)
				       (adjoin-set3 x (left-branch set))
				       (right-branch set)))
		((> x (entry set))
			(make-tree (entry set)
				       (left-branch set)
				       (adjoin-set3 x (right-branch set))))))

;2.63
(define (tree->list-1 tree)
	(if (null? tree)
		'()
		(append (tree->list-1 (left-branch tree))
				(cons (entry tree)
				  	  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
				          (cons (entry tree)
				          	    (copy-to-list (right-branch tree)
				          	    	          result-list)))))
	(copy-to-list tree '()))


;2.64
(define (list->tree elements)
	(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
	(if (= n 0)
		(cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            	  (let ((this-entry (car non-left-elts))
                  		(right-result (partial-tree (cdr non-left-elts)
                               		  right-size)))
              	    (let ((right-tree (car right-result))
                          (remaining-elts (cdr right-result)))
               		  (cons (make-tree this-entry left-tree right-tree)
                      		remaining-elts))))))))

(print (list->tree '(1 3 5 7 9 11)))