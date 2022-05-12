(import (r7rs))


;chapter1.3
;高階手続による抽象

(define (cube x) (* x x x))

(define (sum-integers a b)
	(if (> a b)
		0
		(+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
	(if (> a b)
		0
		(+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))


;(define (identity x) x)   built-in

(define (inc n) (+ n 1))


(define (sum-integers2 a b)
	(sum identity a inc b))


(define (sum-cubes2 a b)
	(sum cube a inc b))


(define (pi-sum2 a b)
	(define (term x)
		(/ 1.0 (* x (+ x 2))))
	(define (next x)
		(+ x 4))
	(sum term a next b))


(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
		dx))

(print (integral cube 0 1 0.01))
(print (integral cube 0 1 0.001))


;1.29
(define (integral-simpson f a b n)
	(define h (/ (- b a) n))
	(define (yk k) (f (+ a (* k h))))
	(define (term x)
		(cond
			((or (= x 0) (= x n)) (yk x))
			((odd? x) (* 4 (yk x)))
			(else (* 2 (yk x)))))
	(/ (* h (sum term 0 inc n)) 3.0))

(print (integral-simpson cube 0 1 100))
(print (integral-simpson cube 0 1 1000))


;1.30
;sumの反復的プロセス
(define (sum2 term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ (term a) result))))
	(iter a 0))


;1.31
(define (product term a next b)
	(if (> a b)
		1
		(* (term a) (product term (next a) next b))))


(define (factorial n)
	(product identity 1 inc n))

(print (factorial 6))


(define (product2 term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* (term a) result))))
	(iter a 1))


(define (factorial2 n)
	(product2 identity 1 inc n))

(print (factorial2 6))


;1.32
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a) 
			(accumulate combiner null-value term (next a) next b))))


(define (sum3 term a next b)
	(accumulate + 0 term a next b))


(define (sum-integers3 a b)
	(sum3 identity a inc b))

(print (sum-integers3 1 5))


(define (product3 term a next b)
	(accumulate * 1 term a next b))


(define (factorial3 n)
	(product3 identity 1 inc n))

(print (factorial3 6))


(define (accumulate2 combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (term a)))))
	(iter a null-value))


(define (sum4 term a next b)
	(accumulate2 + 0 term a next b))


(define (sum-cubes a b)
	(sum4 cube a inc b))

(print (sum-cubes 1 3))


;1.33
(define (filtered-accumulate fcond combiner null-value term a next b)
	(define (iter a result)
		(cond 
			((> a b) result)
			((fcond a) (iter (next a) (combiner result (term a))))
			(else (iter (next a) result))))
	(iter a null-value))


(define (smallest-divisor n)
	(define (find-divisor n i)
		(cond
			((> (square i) n) n)
			((= (remainder n i) 0) i)
			(else (find-divisor n (+ i 1)))))
	(find-divisor n 2))


(define (prime? n)
	(if (< n 2)
		#f
		(= n (smallest-divisor n))))


;a ~ bの素数の二乗の和
(define (sum-square-primes a b)
	(filtered-accumulate prime? + 0 square a inc b))

(print (sum-square-primes 1 10))


;nと互いに素でnより小さい正の整数の積
(define (product-coprimes-under n)
	(define (coprime? i)
		(print (= (gcd i n) 1))
		(print i)
		(= (gcd i n) 1))
	(filtered-accumulate coprime? * 1 identity 1 inc n))

(print (product-coprimes-under 10))