(import (r7rs))

;2.17
(define (last-pair ls)
	(let ((l (cdr ls)))
		(if (null? l)
			(car ls)
			(last-pair l))))

(print (last-pair '(23 72 149 34)))


;2.18
(define (my-reverse ls)
	(if (null? ls)
		'()
		(append (my-reverse (cdr ls)) (list (car ls)))))

(print (my-reverse '(1 4 9 16 25)))
(print (reverse '(1 4 9 16 25)))   ;reverse built-in


;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coin-values)
	(car coin-values))

(define (except-first-denomination coin-values)
	(cdr coin-values))

(define (no-more? coin-values)
	(null? coin-values))

(define (cc amount coin-values)
	(cond
		((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else 
			(+ (cc amount
				   (except-first-denomination coin-values))
			   (cc (- amount
			   	      (first-denomination coin-values))
			        coin-values)))))

(print (cc 100 us-coins))


;2.20
(define (filter f? ls)
	(define (iter ret l)
		(cond
			((null? l) ret)
			((f? (car l)) (iter (cons (car l) ret) (cdr l)))
			(else (iter ret (cdr l)))))
	(iter '() ls))


(define (same-parity x . ls)
	(if (odd? x)
		(cons x (filter odd? ls))
		(cons x (filter even? ls))))


(print (same-parity 1 2 3 4 5 6 7))
(print (same-parity 2 3 4 5 6 7))


;2.21
(define (square-list0 ls)
	(if (null? ls)
		'()
		(cons (square (car ls)) (square-list0 (cdr ls)))))


(define (square-list ls)
	(map square ls))

(print (square-list0 '(1 2 3 4)))
(print (square-list '(1 2 3 4)))


;2.22
(define (square-list2 ls)
	(define (iter ret l)
		(if (null? l)
			ret
			(iter (append ret (list (square (car l))))
				  (cdr l))))
	(iter '() ls))

(print (square-list2 '(1 2 3 4)))


;2.23
(define (for-each2 f ls)
	(if (not (null? ls))
		(begin (f (car ls)) (for-each f (cdr ls)))))

(for-each2 (lambda (x) (newline) (display x))
	       (list 57 321 88))

(for-each (lambda (x) (newline) (display x))  ;for-each built-in
	      (list 57 321 88))
(newline)


;階層構造
;再帰は木構造を扱う自然な道具
(define (count-leaves x)
	(cond
		((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leaves (car x))
			     (count-leaves (cdr x))))))

(print (count-leaves (list (list 1 2) 3 (list 4) 5)))


;2.25
(print (cadr (caddr (list 1 3 (list 5 7) 9))))
(print (caar (list (list 7))))


;2.28
(define (fringe x)
	(cond
		((null? x) '())
		((not (list? (car x))) (cons (car x) (fringe (cdr x))))
		(else (append (fringe (car x)) (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))
(print (fringe x))
(print (fringe (list x x)))


;2.29
(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

(define (left-branch mobile)
	(car mobile))

(define (right-branch mobile)
	(cadr mobile))

(define (branch-length branch)
	(car branch))

(define (branch-structure branch)
	(cadr branch))


(define (total-weight mobile)
	(define (iter branch)
		(let ((struct (branch-structure branch)))
			(if (number? struct) 
				struct
				(+ (iter (left-branch struct))
				   (iter (right-branch struct))))))
	(+ (iter (left-branch mobile)) (iter (right-branch mobile))))

(define mob 
	(make-mobile 
		(make-branch 10 (make-mobile 
							(make-branch 8 10)
			                (make-branch 5 6)))
		(make-branch 15 (make-mobile 
							(make-branch 3 6)
		                    (make-branch 10 (make-mobile 
		                    					(make-branch 30 30)
		                             	        (make-branch 20 15)))))))

(print (total-weight mob))


(define (branch-torque branch)
	(define (iter branch totallen)
		(let ((struct (branch-structure branch)))
			(if (number? struct) 
				(* totallen struct)
				(+ (iter (left-branch struct) 
						 (+ totallen (branch-length branch)))
				   (iter (right-branch struct)
				   		 (+ totallen (branch-length branch)))))))
	(iter branch 0))


(define (branch-torque branch)
	(let ((struct (branch-structure branch)))
		(if (number? struct)
			(* (branch-length branch) struct)
			(* (branch-length branch) (total-weight struct)))))


(define (balanced-part-mobile? mobile)
	(= (branch-torque (left-branch mobile))
	   (branch-torque (right-branch mobile))))


(define (balanced-mobile? mobile)
	(if (number? mobile) 
		#t
		(and (balanced-part-mobile? mobile)
			 (balanced-mobile? (branch-structure (left-branch mobile)))
			 (balanced-mobile? (branch-structure (right-branch mobile))))))


(define mob2 
	(make-mobile 
		(make-branch 3 (make-mobile 
							(make-branch 2 3)
			                (make-branch 3 2)))
		(make-branch 1 (make-mobile 
							(make-branch 4 5)
		                    (make-branch 2 (make-mobile 
		                    					(make-branch 8 5)
		                             	        (make-branch 8 5)))))))

(print (balanced-mobile? mob))
(print (balanced-mobile? mob2))


;2.30
(define (square-tree tree)
	(cond 
		((null? tree) '())
		((list? (car tree)) 
			(cons (square-tree (car tree)) (square-tree (cdr tree))))
		(else
			(cons (square (car tree)) (square-tree (cdr tree))))))


;mapは並びを扱う強力な抽象
;map+再帰は木を扱う強力な抽象
(define (square-tree2 tree)
	(map (lambda (sub-tree)
			(if (pair? sub-tree)
				(square-tree2 sub-tree)
				(square sub-tree)))
		tree))

(define t1
	(list 1
		(list 2 (list 3 4) 5)
		(list 6 7)))

(print (square-tree t1))
(print (square-tree2 t1))


;2.31
;更に抽象化
(define (tree-map f tree)
	(map (lambda (sub-tree)
			(if (pair? sub-tree)
				(tree-map f sub-tree)
				(f sub-tree)))
		tree))

(define (square-tree3 tree) (tree-map square tree))

(print (square-tree3 t1))


