(import (r7rs))

;ストリーム
(define a (delay (car '(1 2 3))))
(print (force a))

(define (memo-proc proc)
	(let ((already-run? #f) (result #f))
		(lambda ()
			(if (not already-run?)
				(begin (set! result (proc))
					   (set! already-run? #t)
					   result)
				result))))

(define (delay expo)
	(memo-proc (lambda () expo)))

(define (force delayed-object)
	(delayed-object))

(define-syntax delay
	(syntax-rules ()
    	((_ exp) (lambda () exp))))

(define-syntax cons-stream
	(syntax-rules ()
		((_ a b) (cons a (delay b)))))

(define (stream-car stream)
	(car stream))

(define (stream-cdr stream)
	(force (cdr stream)))


(define (stream-null? stream)
	(null? stream))

(define the-empty-stream '())

(define (stream-enumerate-interval low high)
	(if (> low high)
    	the-empty-stream
    	(cons-stream
    		low
    		(stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
	(if (= n 0)
    	(stream-car s)
    	(stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
	(if (stream-null? s)
    	'done
    	(begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
	(newline)
	(display x))

(define (display-stream s)
  	(stream-for-each display-line s))

(define (stream-filter pred stream)
	(cond 
		((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream)) 
        	(cons-stream (stream-car stream)
        		(stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;3.50
(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map
				(cons proc (map stream-cdr argstreams))))))

;3.51
(define (show x)
	(display-line x)
	x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(newline)
(print (stream-ref x 5))
(print (stream-ref x 7))


;3.52
(define sum0 0)

(define (accum x)
	(set! sum0 (+ x sum0))
	sum0)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))


(print (stream-ref y 7))
(display-stream z)
(newline)

;無限ストリーム
(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
	(= (remainder x y) 0))

(print integers)

(define no-sevens
	(stream-filter (lambda (x) (not (divisible? x 7)))
		integers))

(define (fibgen a b)
	(cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(print (stream-ref integers 3))
(print (stream-ref fibs 3))


(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
	(stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
	(cons-stream 0
		(cons-stream 1
			(add-streams (stream-cdr fibs)
				fibs))))

(print (stream-ref integers 3))
(print (stream-ref fibs 3))


(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))


;3.53
(define s (cons-stream 1 (add-streams s s)))
#|
1
2
4
8
16
|#

;3.54
(define (mul-streams s1 s2)
	(stream-map * s1 s2))

(define factorials
	(cons-stream 1 (mul-streams integers (stream-cdr integers))))

(print (stream-ref factorials 0))
(print (stream-ref factorials 1))
(print (stream-ref factorials 2))
(print (stream-ref factorials 3))
(print (stream-ref factorials 4))


;3.55
(define (partial-sums s)
	(define s1 (cons-stream (stream-car s) 
		(add-streams (stream-cdr s) s1)))
	s1)

(define ss (partial-sums integers))

;3.56
(define (merge s1 s2)
	(cond
		((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2)))
				(cond
					((< s1car s2car)
						(cons-stream s1car (merge (stream-cdr s1) s2)))
					((> s1car s2car)
						(cons-stream s2car (merge s1 (stream-cdr s2))))
					(else
						(cons-stream s1car
							(merge (stream-cdr s1)
								(stream-cdr s2)))))))))

(define S (cons-stream 1 
	(merge (scale-stream S 2)
		(merge (scale-stream S 3)
			(scale-stream S 5)))))

;3.59
(define (integrate-series s)
	(let ((n 0))
    	(stream-map (lambda (x) (set! n (+ n 1)) (/ x n)) s)))

(define exp-series
	(cons-stream 1 (integrate-series exp-series)))


(define (average x y)
	(/ (+ x y) 2))

(define (sqrt-improve guess x)
	(average guess (/ x guess)))

(define (sqrt-stream x)
	(define guesses
		(cons-stream 1.0
			(stream-map (lambda (guess)
							(sqrt-improve guess x))
			            guesses)))
	guesses)

;3.64
(define (stream-limit stream tolerance)
	(let ((first (stream-car stream)) 
		  (rest (stream-cdr stream)))
		(if (< (abs (- first (stream-car rest))) tolerance)
        	(stream-car rest)
        	(stream-limit rest tolerance))))

(define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance))


;対の無限のストリーム
(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream (stream-car s1)
			         (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(interleave
			(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
			(pairs (stream-cdr s) (stream-cdr t)))))


;3.69
(define (triples s t u)
	(cons-stream
		(list (stream-car s) (stream-car t) (stream-car u))
		(interleave
			(stream-map (lambda (x) (cons (stream-car s) x))
				(stream-map (lambda (y) (list (stream-car t) y))
					(stream-cdr u)))
			(triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (triples s t u)
	(cons-stream
		(list (stream-car s) (stream-car t) (stream-car u))
		(interleave
			(stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
			(triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (pythagoras s)
	(stream-filter 
		(lambda (x) 
			(= (+ (square (car x)) (square (cadr x)))
			   (square (caddr x))))
		s))

;(display-stream (pythagoras (triples integers integers integers)))


;3.70
(define (merge-weighted s1 s2 weight)
	(cond
		((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2)))
				(if (< (apply weight s1car) (apply weight s2car))
              		(cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
              		(cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))


;信号としてのストリーム
(define (integral integrand initial-value dt)
	(define int
		(cons-stream initial-value
			(add-streams (scale-steram integrand dt)
				int)))
	int)

;3.73
(define (RC r c t)
	(lambda (i-stream v0)
		(cons-stream v0
			(add-stream (scale-stream i-stream r)
			     (integral (scale-stream i-stream (/ 1.0 c)) 0 t)))))

(define RC1 (RC 5 1 0.5))


;3.74
(define (sign-change-detector x1 x2)
	(cond
		((and (< 0 x1) (> 0 x2)) -1)
		((and (> 0 x1) (< 0 x2)) 1)
		(else 0)))

(define zero-crossings
	(stream-map sign-change-detector 
		sense-data
		(cons-stream 0 dense-data)))

;3.75
(define (make-zero-crossings input-stream last-value last-avpt)
	(let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
		(cons-stream (sign-change-detector avpt last-avpt)
			(make-zero-crossings (stream-cdr input-stream)
				(stream-car input-stream)
				avpt))))

;3.76
(define (smooth s)
	(stream-cons (stream-car s)
		(stream-map (lambda (x y) (/ (+ x y) 2)) 
			(stream-cdr s) s)))

(define (make-zero-crossings input-stream last-value)
	(cons-stream (sign-change-detector (stream-car input-stream) last-avpt)
		(make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))


;3.81
(define (rand-update x)
	(+ x 2))

(define (random s init)
	(define (iter s rand)
		(if (stream-null s)
			the-empty-stream
			(cons-stream rand 
				(cond
					((eq? (stream-car s) 'generate)
						(iter (stream-cdr s) (rand-update rand)))
					((eq? (stream-car s) 'reset)
						(iter (stream-cdr s) init))
					(else (error 'error))))))
	(iter s init))
