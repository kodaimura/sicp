(import (r7rs))


;分子が整数n、分母が整数dの有理数
(define (make-rat0 n d)
	(cons n d))

;約分付きmake-rat
(define (make-rat1 n d)
	(let ((g (gcd n d)))
		(cons (/ n g) (/ d g))))

;有理数xの分子
(define (numer x)
	(car x))

;有理数xの分母
(define (denom x)
	(cdr x))


(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))


;2.1
;負の符号に対応したmake-rat
(define (make-rat n d)
	(let ((g (gcd n d))
		  (dabs (abs d)))
		(cons (/ (* (/ d dabs) n) g) (/ dabs g))))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 -2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 2))


(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
		         (* (numer y) (denom x)))
	          (* (denom x) (denom y))))


(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
		         (* (numer y) (denom x)))
	          (* (denom x) (denom y))))


(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
		      (* (denom x) (denom y))))


(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
		      (* (denom x) (numer y))))


(define (equal-rat? x y)
	(= (* (numer x) (denom y))
	   (* (numer y) (denom y))))


(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))


;2.2
;平面上の線分
(define (make-point x y)
	(cons x y))


(define (x-point p)
	(car p))


(define (y-point p)
	(cdr p))


(define (make-segment start-p end-p)
	(cons start-p end-p))


(define (start-segment seg)
	(car seg))


(define (end-segment seg)
	(cdr seg))


(define (midpoint-segment seg)
	(let ((sp (start-segment seg))
		  (ep (end-segment seg)))
		(cons (/ (+ (x-point sp) (x-point ep)) 2)
			  (/ (+ (y-point sp) (y-point ep)) 2))))


(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))


(define seg1 (make-segment (make-point 0 0) (make-point 5 5)))
(define seg2 (make-segment (make-point -3 -3) (make-point 5 5)))
(define seg3 (make-segment (make-point 1 8) (make-point 10 4)))
(print-point (midpoint-segment seg1))
(print-point (midpoint-segment seg2))
(print-point (midpoint-segment seg3))


;2.3
;長方形
(define (distance-points p1 p2)
	(sqrt (+ (square (- (x-point p1) (x-point p2)))
		     (square (- (y-point p1) (y-point p2))))))


(define (make-rectangle a b c d)
	(let ((bd (square (distance-points b d))))
		(if (and (= (+ (square (distance-points a b))
			           (square (distance-points a d)))
		             bd)
		         (= (+ (square (distance-points c b))
			           (square (distance-points c d)))
		             bd))
		    (list a b c d)
		    (error "error"))))


(define (a-vertex rec)
	(car rec))

(define (b-vertex rec)
	(cadr rec))

(define (c-vertex rec)
	(caddr rec))

(define (d-vertex rec)
	(cadddr rec))


(define (perimeter-rectangle rec)
	(+ (* 2 (distance-points  (a-vertex rec) (b-vertex rec)))
	   (* 2 (distance-points  (a-vertex rec) (d-vertex rec)))))


(define (perimeter-area rec)
	(* (distance-points  (a-vertex rec) (b-vertex rec))
	   (distance-points  (a-vertex rec) (d-vertex rec))))


(define rectangle 
	(make-rectangle (make-point 0 0)
	                (make-point 0 3)
	                (make-point 5 3)
	                (make-point 5 0)))

(print rectangle)
(print (perimeter-rectangle rectangle))
(print (perimeter-area rectangle))


;2.4
;対をconsという「手続き」として実装
;実際のほとんどのLispでは下記のようではなく、「対」を直接実装している
(define (cons2 x y)
	(lambda (m)
		(cond 
			((= m 0) x)
			((= m 1) y)
			(else (error "Argument not 0 or 1 --CONS" m)))))

(define (car2 z) (z 0))

(define (cdr2 z) (z 1))

(define a (cons2 1 2))
(print a)
(print (car2 a))
(print (cdr2 a))


(define (cons3 x y)
	(lambda (m)
		(m x y)))

(define (car3 z)
	(z (lambda (p q) p)))

(define (cdr3 z)
	(z (lambda (p q) q)))

(define b (cons3 1 2))
(print b)
(print (car3 b))
(print (cdr3 b))


;2.5
;aとbの対を2^a*3^bで表現
(define (cons4 x y)
	(* (expt 2 x) (* (expt 3 y))))

(define (car4 z)
	(define (iter n c)
		(if (= (remainder n 2) 0)
			(iter (/ n 2) (+ c 1))
			c))
	(iter z 0))

(define (cdr4 z)
	(define (iter n c)
		(if (= (remainder n 3) 0)
			(iter (/ n 3) (+ c 1))
			c))
	(iter z 0))

(define c (cons4 3 5))
(print c)
(print (car4 c))
(print (cdr4 c))


;2.6
;Church数(チャーチ数)
;手続きlambdaを操作できる言語では、0と1を足す演算を数を使わずに表現できる


;2.7
;区間算術演算
(define (make-interval a b)
	(cons a b))

(define (upper-bound interval)
	(max (car interval) (cdr interval)))

(define (lower-bound interval)
	(min (car interval) (cdr interval)))


(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
		           (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
		  (p2 (* (lower-bound x) (upper-bound y)))
		  (p3 (* (upper-bound x) (lower-bound y)))
		  (p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4)
			           (max p1 p2 p3 p4))))


(define (div-interval x y)
	(mul-interval x
		          (make-interval (/ 1.0 (upper-bound y))
		          	             (/ 1.0 (lower-bound y)))))

(print (add-interval (make-interval 3 6) (make-interval 8 10)))
(print (mul-interval (make-interval 3 6) (make-interval 8 10)))
(print (div-interval (make-interval 3 6) (make-interval 8 10)))
