(import (r7rs))

;ストリーム
(define a (delay (car '(1 2 3))))
(print (force a))

(define (delay expo)
	(lambda () expo))

(define (force delayed-object)
	(delayed-object))

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

(define a (delay (car '(1 2 3))))
(print (force a))


(define (stream-car stream)
	(car stream))

(define (stream-cdr stream)
	(force (cdr stream)))

(define (cons-stream a b)
	(cons a (delay  b)))

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