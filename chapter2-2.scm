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

;2.32
(define (subsets s)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
			(append rest 
					(map (lambda (l)
							(cons (car s) l)) rest)))))

(print (subsets '(1 2 3)))


;2.33
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))


(define (_map p s)
	(accumulate (lambda (x y)
					(cons (p x) y)) '() s))


(define (_append seq1 seq2)
	(accumulate cons seq2 seq1))


(define (_length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(print (_map square '(1 2 3)))
(print (map square '(1 2 3)))
(print (_append '(1 2) '(3 4 5)))
(print (append '(1 2) '(3 4 5)))
(print (length '(1 2 3)))
(print (_length '(1 2 3)))


;2.34
;anX^n + an-1X^n-1 + ... + a1X + a0
;Hornerの方法
;((anX + an-1)X + ... + a1)X + a0
;anから始め、xを掛け、an-1を足し、xを掛け、a0まで繰り返し
(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms)
					(+ this-coeff (* x higher-terms)))
				0
				coefficient-sequence))

;x=2 1 + 3x + 5x^3 + x^5
(print (horner-eval 2 (list 1 3 0 5 0 1)))


;2.35
(define (count-leaves2 t)
	(accumulate +
				0
				(map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(print (count-leaves2 (list (list 1 2) 3 (list 4) 5)))


;2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		'()
		(cons (accumulate op init (map car seqs))
			  (accumulate-n op init (map cdr seqs)))))

(print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))


;2.38
(define fold-right accumulate)

(define (fold-left op init seq)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				  (cdr rest))))
	(iter init seq))

(print (fold-right / 1 (list 1 2 3)))
(print (fold-left / 1 (list 1 2 3)))
(print (fold-right list '() (list 1 2 3)))
(print (fold-left list '() (list 1 2 3)))


;2.39
(define (reverse1 seq)
	(fold-right (lambda (x y) (append y (list x)))
				'()
				seq))


(define (reverse2 seq)
	(fold-left (lambda (x y) (append (list y) x))
				'()
				seq))

(print (reverse1 (list 1 2 3 4 5)))
(print (reverse2 (list 1 2 3 4 5)))


;写像
(define (enumerate-interval low high)
	(if (> low high)
		'()
		(cons low (enumerate-interval (+ low 1) high))))

(print
(accumulate append
			'()
			(map (lambda (i)
					(map (lambda (j) (list i j))
					 	 (enumerate-interval 1 (- i 1))))
				 (enumerate-interval 1 6)))
)

(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))


(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime? x)
	(define (iter n) 
		(cond 
			((< x (square n)) #t)
			((zero? (remainder x n)) #f)
			(else
				(iter (+ n 2)))))
	(if (or (< x 2) (and (not (= x 2)) (even? x))) #f (iter 3)))


(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum?
			(flatmap
				(lambda (i)
					(map (lambda (j) (list i j))
						 (enumerate-interval 1 (- i 1))))
				(enumerate-interval 1 n)))))

(print (prime-sum-pairs 6))


;2.40
(define (unique-pairs n)
	(accumulate append
			'()
			(map (lambda (i)
		 			(map (lambda (j) (list i j))
			  	 (enumerate-interval 1 (- i 1))))
		 	(enumerate-interval 1 n))))


(define (prime-sum-pairs2 n)
	(map make-pair-sum
		 (filter prime-sum?
			(unique-pairs n))))

(print (unique-pairs 6))
(print (prime-sum-pairs2 6))


;2.41
(define (permutations s)
	(accumulate append 
			    (list (list)) 
			    (map (lambda (x) 
			       		(map (lambda (p) (cons x p))
			       				(permutations (remove x s))))
			       	  s)))

(define (remove item seq)
	(filter (lambda (x) (not (= x item)))
			seq))

(print (permutations (list 1 2 3 4)))


(define (sum-list ls)
	(if (null? ls)
		0
		(+ (car ls) (sum-list (cdr ls)))))


(define (c2q41 n s)
	(filter (lambda (l) (and (= (length l) 3) (= (sum-list l) s)))
		    (permutations (enumerate-interval 1 n))))

(print (c2q41 5 7))

;図形言語
;(define wave2 (beside waev (flip-vert wave)))
;(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
	(let ((painter2 (beside painter (flip-vert painter))))
		(below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (right-split painter (- n 1))))
			(beside painter (below smaller smaller)))))

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let ((up (up-split painter (- n 1)))
			  (right (right-split painter (- n 1))))
			(let ((top-left (beside up up))
				  (bottom-right (below right right))
				  (corner (corner-split painter (- n 1))))
				(beside (below painter top-left)
						(below bottom-right corner))))))

(define (square-limit painter n)
	(let ((quarter (corner-split painter n)))
		(let ((half (beside (flip-horize quarter) quarter)))
			(below (flip-vert half half)))))

;2.44
(define (up-split painer n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smallr smaller)))))

;2.45
(define (split op1 op2)
	(lambda (painter n))
		(if (= n 0)
			painter
			(let ((smaller (split painter (- n 1))))
				(op1 painter (op2 smallr smaller)))))

;2.46
(define (make-vect x y)
	(list x y))

(define (xcor-vect vect)
	(car vect))

(define (ycor-vect vect)
	(cadr vect))

(define (add-vect v1 v2)
	(make-vect (+ (xcor-vect v1) (xcor-vect v2))
			   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
	(make-vect (- (xcor-vect v1) (xcor-vect v2))
			   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
	(make-vect (* s (xcor-vect v)) (* s (ycor-vect))))


(define (frame-coord-map frame)
	(lambda (v)
		(add-vect
			(origin-frame frame)
			(add-vect (scale-vect (xcor-vect v)
					  			  (edge1-frame frame))
					  (scale-vect (ycor-vect v)
					  			  (edge2-frame frame))))))


;2.47
(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2))

(define (origin-frame frame)
	(car frame))

(define (edge1-frame frame)
	(cadr frame))

(define (edge2-frame frame)
	(caddr frame))


;2.48
(define (make-segment s-seg e-seg)
	(list s-seg e-seg))

(define (start-segment seg)
	(car seg))

(define (end-segment seg)
	(cadr seg))


;2.49
(define (segment->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
				(draw-line
					((frame-coord-map frame) (start-segment segment))
					((frame-coord-map frame) (end-segment segment)))
			(segment-list)))))


(define outline-painter 
  (segments->painter 
    (list (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 0 1) (make-vect 1 1))
          (make-segment (make-vect 1 1) (make-vect 1 0))
          (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define X-painter
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond-painter
  (segments->painter 
    (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
          (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
          (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
          (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))
