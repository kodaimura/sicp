(import (r7rs))
(import (chicken sort))

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

;2.65
(define (union-set-tree tree1 tree2)
	(list->tree (union-set (tree->list-1 tree1))))

(define (intersection-set-tree tree1 tree2)
	(list->tree (intersection-set (tree->list-1 tree1)
		                          (tree->list-1 tree2))))


;Huffman木
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
	(list left
		  right
		  (append (symbols left) (symbols right))
		  (+ (weight left) (weight right))))

;復号化
(define (choose-branch bit branch)
	(cond 
		((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch
					(choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch)
						  (decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define huf
	(make-code-tree 
		(make-leaf 'A 8)
		(make-code-tree 
			(make-code-tree 
				(make-leaf 'B 3)
				(make-code-tree
					(make-leaf 'C 1)
					(make-leaf 'D 1)))
			(make-code-tree
				(make-code-tree
					(make-leaf 'E 1)
					(make-leaf 'F 1))
				(make-code-tree
					(make-leaf 'G 1)
					(make-leaf 'H 1))))))

(print huf)
(print (decode (list 1 0 0 0 1 0 1 0) huf))


;重み付き要素の集合
(define (adjoin-set x set)
	(cond
		((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set)
			  (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
			(adjoin-set (make-leaf (car pair)
				                   (cadr pair))
						(make-leaf-set (cdr pairs))))))

(print (make-leaf-set (list (list 'A 4) (list 'C 1) (list 'B 2) (list 'D 1))))


;2.67
(define sample-tree
	(make-code-tree 
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree
				(make-leaf 'D 1)
				(make-leaf 'C 1)))))

(define sample-message
	'(0 1 1 0 0 1 0 1 0 1 1 1 0))

(print (decode sample-message sample-tree))

;2.68
(define (encode-symbol symbol tree)
	(if (leaf? tree)
		'()
		(let ((left (left-branch tree))
		  	  (right (right-branch tree)))
			(cond
				((element-of-set? symbol (symbols left))
					(cons 0 (encode-symbol symbol left)))
				((element-of-set? symbol (symbols right))
					(cons 1 (encode-symbol symbol right)))
				(else 
					(error "bad symbol -- ENCODE=SYMBOL" symbol))))))

(define (encode message tree)
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
			    (encode (cdr message) tree))))


(print (encode (list 'A 'D 'A 'B 'B 'C 'A) sample-tree))

;2.69
(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
	(if (= 1 (length leaf-set))
		(car leaf-set)
		(let ((sorted (sort leaf-set (lambda (x y) (< (weight x) (weight y))))))
       		(successive-merge 
        		(cons (make-code-tree (car sorted) (cadr sorted))
              		  (cddr sorted))))))

(print (generate-huffman-tree 
	(list (list 'A 4) (list 'C 1) (list 'B 2) (list 'D 1))))

;2.70
(define hufftree
	(generate-huffman-tree
		(list (list 'a 2) (list 'boom 1) (list 'get 2) (list 'job 2)
			  (list 'na 16) (list 'sha 3) (list 'yip 9) (list 'wah 1))))

(define enc (encode (list 'get 'a 'job 'sha 'na 'na
	                 'na 'na 'na 'na 'na 'na) hufftree))

(print enc)
(print (decode enc hufftree))
