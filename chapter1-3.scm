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


;不動点
;xがf(x)=xを満たすとき、xを関数fの不動点という
;最初の予測値から、fを値があまり変わらなくなるまで繰り返し作用させることで不動点を見つける
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) 0.00001))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(print (fixed-point cos 1.0))  ;cosx=xとなるx
(print (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)) 

;不動点プロセスを平方根の計算に応用
;y^2=xとなるyを探す -> y=x/yの不動点を探す
;ただし、下の記述は予測値の振動が収束しない
#|
(define (sqrt1 x)
	(fixed-point (lambda (y) (/ x y))
		1.0))
|#


;平均緩和法(average damping)
;不動点探索で収束を助けることが多い
;予測値の大きな変化を防ぐ
;次の予測値をx/yではなく、1/2(y+x/y)とする(両辺にyを足して2で割る)
(define (sum-of ls)
	(define (iter result ls)
		(if (null? ls)
			result
			(iter (+ (car ls) result) (cdr ls))))
	(iter 0 ls))


(define (average . a)
	(/ (sum-of a) (length a)))


(define (sqrt2 x)
	(fixed-point (lambda (y) (average y (/ x y)))
		1.0))

(print (sqrt2 2))
(print (sqrt2 9))

;1.35
(print (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
(print (fixed-point (lambda (y) (average y (+ 1 (/ 1 y)))) 1.0))


;1.36
;x^x=1000の解
;-> x=log(1000)/log(x)の不動点を求める
(print "Fixed-Point")
(define (fixed-point-print f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) 0.00001))
	(define (try guess)
		(print guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(print (try first-guess)))

(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 5.0)

(print "Fixed-Point (平均緩和)")
(fixed-point-print 
  (lambda (x) (average x (/ (log 1000) (log x)))) 5.0)


;ニュートン法でもやってみる
;f(x)=0となるxを求める
;Xn+1=Xn-f(Xn)/f'(Xn)
;x^x=1000の解
;x^x-1000=0 -> f(x)=x^x-1000
;Xn+1=Xn-(x^x-1000)/x^x*(logx+1)
(print "Newton")
(define (newton-print f first-guess)
	(fixed-point-print f first-guess)
	(define (close-enough? guess x)
		(< (abs (- guess x)) 0.00001))
	(define (try guess)
		(print guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(print (try first-guess)))

(newton-print 
	(lambda (x) 
		(- x (/ (- (expt x x) 1000) 
			    (* (expt x x) (+ (log x) 1)))))
	5.0)

#|
(define (newton-print f first-guess)
	(fixed-point-print f first-guess)
|#


;1.37
(define (cont-frac ni di k)
	(define (iter i k)
		(if (> i k)
			0
			(/ (ni i) (+ (di i) (iter (+ i 1) k)))))
	(iter 1 k))

(print (cont-frac (lambda (i) 1.0)
	              (lambda (i) 1.0)
	              100))


(define (cont-frac2 ni di k)
	(define (iter result i)
		(if (zero? i)
			result
			(iter (/ (ni i) (+ (di i) result)) (- i 1))))
	(iter 0 k))

(print (cont-frac2 (lambda (i) 1.0)
	               (lambda (i) 1.0)
	               100))


;1.38
(define (e-frac i)
	(let ((x (+ i 1)))
		(if (= (remainder x 3) 0)
			(* (/ x 3) 2)
			1)))

(print (cont-frac (lambda (i) 1.0) 
	              e-frac 
	              100))


;1.39
(define (tan-cf x k)
	(* 1.0 (cont-frac (lambda (i) (if (= i 1) x (* -1 x x))) 
		              (lambda (i) (- (* 2 i) 1)) 
		              k)))

(print (tan-cf 1 100))
(print (tan-cf 1.0472 100))
(print (tan-cf -1 100))


;微分
(define dx 0.00001)

(define (derive g)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x))
			dx)))


(define (cube x)
	(* x x x))

;f(x)=x^3 ... f'(5)
(print ((derive cube) 5))

;g(x)-> f(x)=x-g(x)/g'(x) 
(define (newton-transform g)
	(lambda (x)
		(- x (/ (g x) ((derive g) x)))))


(define (newtons-method g guess)
	(fixed-point (newton-transform g) guess))


(define (sqrt3 x)
	(newtons-method (lambda (y) (- (square y) x)) 1.0))

(print (sqrt3 2))
(print (sqrt3 9))

;再1.36
(print (newtons-method (lambda (x) (- (expt x x) 1000)) 5.0))

;1.40
;x^3+ax^2+bx+c=0
(define (cubic a b c)
	(lambda (x)
		(+ (cube x) (* a (square x)) (* b x) c)))

(print (newtons-method (cubic 10 5 3) 1))

;1.41
(define (double f)
	(lambda (x) (f (f x))))

(print (((double (double double)) inc) 5))

;1.42
(define (my-compose f g)
	(lambda (x) (f (g x))))

(print ((my-compose square inc) 6))

;composeはchicken-schemeに既に定義済み
(print ((compose square inc) 6))

;1.43
(define (repeated f n)
	(if (zero? n)
		(lambda (x) x)
		(compose f (repeated f (- n 1)))))

(print ((repeated square 2) 5))


;1.44
;平滑化
;xでの値がf(x-dx),f(x),f(x+dx)の平均である関数
(define dx2 0.001)


(define (smooth f)
	(lambda (x)
		(average (f x) (f (- x dx2)) (f (+ x dx2)))))


(define (n-fold-smooth f n)
	(repeated (smooth f) n))

(print ((n-fold-smooth square 5) 2))


;1.45
(define (average-damp f)
	(lambda (x) (average x (f x))))

;c -> 平均緩和の回数
(define (n-root n x k)
	(fixed-point ((repeated average-damp k) (lambda (y) (/ x (expt y (- n 1)))))
		1.0))

(print (n-root 2 4 1))

(print (n-root 3 8 1))

;(print (n-root 4 16 1)) ;NG
(print (n-root 4 16 2))

;(print (n-root 5 32 1)) ;NG
(print (n-root 5 32 2))

(print (n-root 6 64 1))
;(print (n-root 6 64 2))

(print (n-root 7 128 1))
;(print (n-root 7 128 2))

(print (n-root 8 256 1))
;(print (n-root 8 256 3))

(print (n-root 9 512 1))
;(print (n-root 9 512 2))

(print (n-root 10 1024 1))


;1.46
;反復改良法
;不動点やニュートン法を一般的に
(define (iterative-improve enough? improve)
	(lambda (first-guess)
		(define (iter g)
			(if (enough? g)
				g
				(iter (improve g))))
		(iter first-guess)))


(define (sqrt4 x)
	((iterative-improve (lambda (g) (< (abs (- (square g) x)) 0.001))
		                (lambda (g) (average g (/ x g)))) 
	 1.0))

(print (sqrt4 2))
(print (sqrt4 9))


(define (fixed-point2 f first-guess)
	((iterative-improve (lambda (g) (< (abs (- g (f g))) 0.00001))
		                f)
	 first-guess))

(print (fixed-point2 cos 1.0))
(print (fixed-point2 (lambda (y) (+ (sin y) (cos y))) 1.0))
