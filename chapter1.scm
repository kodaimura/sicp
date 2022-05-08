(import (r7rs))
(import (chicken random))

;chapter1
;手続きによる抽象の構築


;1.3
;三つの引数の大きい二つの二乗の和
(define (c1q2 a b c)
	(let* ((x (max a b)) (y (max x c)))
		(+ (* x x) (* y y))))

(print (c1q2 1 2 3))


;Newton法による平方根
;数xの平方根の値の予測値yがあれば、yとxの平均値をとるとさらに良い予測値が取れる

(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))

(define (improve guess x)
	(average guess (/ x guess)))

(define (average x y)
	(/ (+ x y) 2))

(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))

(define (newtonsqrt x)
	(sqrt-iter 1.0 x))

(print (newtonsqrt 9))
(print (newtonsqrt 137))
(print (sqrt 9))  
(print (sqrt 137))


;ブロック構造
;補助手続きを内部関数とし、名前保護する

(define (newtonsqrt2 x)
	(define (good-enough? guess x)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess x)
		(average guess (/ x guess)))
	(define (sqrt-iter guess x)
		(if (good-enough? guess x)
			guess
			(sqrt-iter (improve guess x) x)))
	(sqrt-iter 1.0 x))

(print (newtonsqrt2 9))
(print (newtonsqrt2 137))


;1.8
;Newton法による立方根の近似
(define (cube x)
	(* x x x))

(define (newtoncurt x)
	(define (good-enough? guess x)
		(< (abs (- (cube guess) x)) 0.001))
	(define (improve guess x)
		(/ (+ (/ x (square guess)) (* 2 guess)) 3))
	(define (curt-iter guess x)
		(if (good-enough? guess x)
			guess
			(curt-iter (improve guess x) x)))
	(curt-iter 1.0 x))

(print (newtoncurt 27))
(print (newtoncurt 137))


;再帰と反復
;n!

;再帰プロセス
;膨張と収縮からなる
;膨張は遅延演算の列を作る時
;収縮は演算が実際に実行される時
(define (factorial n)
	(if (= n 1)
		1
		(* n (factorial (- n 1)))))

(print (factorial 6))

;反復的プロセス
;各ステップで覚えておくのはproduct, counter, max_countのみ
;再帰的手続きであることに変わりはない
(define (factorial2 n)
	(define (iter product counter)
		(if (> counter n)
			product
			(iter (* counter product) (+ counter 1))))
	(iter 1 1))

(print (factorial2 6))


;通常の言語の実装は再帰的手続きの実行で消費するメモリは
;反復的プロセスであっても手続き呼び出しの数とともに増加する設計である
;そのためこれらではdo,repeat,for,whileなどの特殊なループ構造で記述する
;schemeでは、反復的プロセスは再帰的手続きで記述しても固定スペースで実行出来る
;この性質の実装を末尾再帰的という(特殊なループ構造は不要)


;木構造再帰
(define (fib n)
	(cond 
		((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))

(print (fib 10))


(define (fib2 n)
	(fib-iter 1 0 n))

(define (fib-iter a b counter)
	(if (= counter 0)
		b
		(fib-iter (+ a b) a (- counter 1))))

(print (fib2 10))


;1.11
;再帰的プロセス
(define (c1q11a n)
	(if (< n 3)
		n
		(+ (c1q11a (- n 1))
		   (* 2 (c1q11a (- n 2)))
		   (* 3 (c1q11a (- n 3))))))

(print (c1q11a 3))
(print (c1q11a 10))

;反復的プロセス
;???


;1.12
;Pascal三角形
;n段i番目の要素
(define (pascal n i)
	(if (or (= i 0) (= i n))
		1
		(+ (pascal (- n 1) (- i 1))
			(pascal (- n 1) i))))

(print (pascal 4 1))
(print (pascal 4 2))


;1.13
;x = (1 + √5)/2
;y = (1 - √5)/2
;fib(n) = (x^n - y^n)/√5 を証明
(define (pow x n)
	(if (= n 1)
		x
		(* x (pow x (- n 1)))))

(define (c1q13 n)
	(/ (- (pow (/ (+ 1 (sqrt 5)) 2) n)
		  (pow (/ (- 1 (sqrt 5)) 2) n))
		(sqrt 5)))

(print (c1q13 1))
(print (fib 1))
(print (c1q13 10))
(print (fib 10))


;1.16
(define (fast-expt b n)
	(cond
		((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(print (fast-expt 2 10))


(define (fast-expt2 b n)
	(define (iter a b c)
		(cond
			((= c 0) a)
			((even? c) (iter a (square b) (/ c 2)))
			(else (iter (* a b) b (- c 1)))))
	(iter 1 b n))

(print (fast-expt2 2 10))


;1.17
(define (multi0 a b)
	(if (= b 0) 
		0
		(+ a (multi0 a (- b 1)))))

(print (multi0 3 4))


(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (multi a b)
	(cond
		((= b 0) 0)
		((= b 1) a)
		((even? b) (double (multi a (halve b))))
		(else (+ a (multi a (- b 1))))))

(print (multi 3 0))
(print (multi 3 1))
(print (multi 3 4))


(define (multi2 a b)
	(define (iter a b)
		(cond
			((= b 0) 0)
			((= b 1) a)
			(else (iter (double a) (halve b)))))
	(if (even? b)
		(iter a b)
		(+ a (iter a (- b 1)))))

(print (multi2 3 0))
(print (multi2 3 1))
(print (multi2 3 4))


;1.19
;Fibonacci数を対数的ステップで計算するアルゴリズム
(define (fibo n)
	(fibo-iter 1 0 0 1 n))

(define (fibo-iter a b p q c)
	(cond
		((= c 0) b)
		((even? c) 
			(fibo-iter 
				a 
				b 
				(+ (* p p) (* q q))
				(+ (* 2 p q) (* q q)) 
				(/ c 2)))
		(else 
			(fibo-iter 
				(+ (* b q) (* a q) (* a p)) 
				(+ (* b p) (* a q)) 
				p 
				q 
				(- c 1)))))

(print (fibo 10))


;最大公約数
;Euclid
(define (gcd0 a b)
	(if (= b 0)
		a
		(gcd0 b (remainder a b))))

(print (gcd0 206 40))


;除数
(define (smallest-divisor n)
	(define (find-divisor n i)
		(cond
			((> (square i) n) n)
			((= (remainder n i) 0) i)
			(else (find-divisor n (+ i 1)))))
	(find-divisor n 2))

;素数判定
(define (prime? n)
	(if (< n 2)
		#f
		(= n (smallest-divisor n))))

(print (prime? 1))
(print (prime? 2))
(print (prime? 4))
(print (prime? 13))


;Fermatテスト

;Fermetの小定理
;nを素数、aをnより小さい正の整数とすると、a^n/nの余りと a/nの余りが同じ
;nが素数でないとき、a^n/nの余りと a/nの余りが同じでないことが多い
;5,3 5^3/3 ... 2 5/3 ... 2

;a^n/nの計算
(define (expmod base ex m)
	(cond 
		((= ex 0) 1)
		((even? ex) (remainder (square (expmod base (/ ex 2) m)) m))
		(else (remainder (* base (expmod base (- ex 1) m)) m))))

(define (fermet-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (pseudo-random-integer (- n 1)))))

(define (fast-prime? n times)
	(cond 
		((= times 0) #t)
		((fermet-test n) (fast-prime? n (- times 1)))
		(else #f)))

(print (fast-prime? 5 2))
(print (fast-prime? 13 2))
(print (fast-prime? 2147483647 1000))


;1.21
(print (smallest-divisor 199))   ;199
(print (smallest-divisor 1999))  ;1999
(print (smallest-divisor 19999)) ;7







