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
	(stream-map (lambda (x) (* x factor)) steram))

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
(print (stream-ref s 0))
(print (stream-ref s 1))
(print (stream-ref s 2))
(print (stream-ref s 3))
(print (stream-ref s 4))


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

(print (stream-ref ss 0))
(print (stream-ref ss 1))
(print (stream-ref ss 2))
(print (stream-ref ss 3))
(print (stream-ref ss 4))