(import (r7rs))

;3.17
(define (count-pairs x)
	(if (not (pair? x))
		0
		(+ (count-pairs (car x))
		   (count-pairs (cdr x))
		   1)))

(define a (cons 'x 'y))
(define b (cons a a))
(define c (cons 'x b))
(define d (cons b b))

(print (count-pairs c))
(print (count-pairs d))


(define (count-pairs x)
	(define ls '())
	(define (count-pairs-aux x)
		(cond 
			((not (pair? x)) 0)
        	((memq x ls) 0)   ;menq ... 判定にeq?(オブジェクトの比較)
          	(else 
          		(begin
          			(set! ls (cons x ls))
                	(+ (count-pairs-aux (car x))
                   	   (count-pairs-aux (cdr x))
                   		1)))))
	(count-pairs-aux x))

(print (count-pairs c))
(print (count-pairs d))


;3.18
(define (circulation? x)
	(define ls '())
	(define (iter x)
		(cond
			((not (pair? x)) #f)
			((memq x ls) #t)
			(else 
				(begin (set! ls (cons x ls)) 
					   (iter (cdr x))))))
	(iter x))


(print (circulation? a))
(set-cdr! a a)
(print (circulation? a))


;3.19
(define (circulation? x)
	(cond
		((not (pair? x)) #f)
		((not (pair? (cdr x))) #f)
		((eq? x (cdr x)) #t)
		(else (circulation? (cdr x)))))

(define e (cons 'x 'y))
(print (circulation? e))
(set-cdr! e e)
(print (circulation? e))


;キュー
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
	(if (empty-queue? queue)
		(error "FRONT called with an empty queue" queue)
		(car (front-ptr queue))))

(define (insert-queue! queue item)
	(let ((new-pair (cons item '())))
		(cond
			((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
				queue)
			(else
				(set-cdr! (rear-ptr queue) new-pair)
				(print queue)
				(set-rear-ptr! queue new-pair)
				queue))))

(define (delete-queue! queue)
	(cond
		((empty-queue? queue)
			(error "DELETE! called with an empty queue" queue))
		(else
			(set-front-ptr! queue (cdr (front-ptr queue)))
			queue)))

;3.21
(define (print-queue queue)
	(print (front-ptr queue)))
