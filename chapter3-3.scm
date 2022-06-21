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

(define q (make-queue))
(print (insert-queue! q 1))
(print (insert-queue! q 2))
(print (insert-queue! q 3))

;3.21
(define (print-queue queue)
	(print (front-ptr queue)))

;3.22
(define (make-queue)
	(let ((front-ptr '())
		  (rear-ptr '()))
		(define (set-front-ptr! item) (set! front-ptr item))
		(define (set-rear-ptr! item) (set! rear-ptr item))
		(define (empty-queue?) (null? front-ptr))
		(define (front-queue)
			(if (empty-queue?) 
			(error "FRONT called with an empty queue")
			(car front-ptr)))
		(define (insert-queue! item)
			(let ((new-item (cons item '())))
				(cond 
					((empty-queue?) 
						(set! front-ptr new-item)
                        (set! rear-ptr new-item))
                    (else (set-cdr! rear-ptr new-item)
                        (set! rear-ptr (cdr rear-ptr))))))
		(define (delete-queue!)
			(cond
				((empty-queue?)
					(error "DELETE! called with an empty queue"))
				(else
					(set-front-ptr! (cdr front-ptr)))))
		(define (print-queue) (print front-ptr))

		(define (dispatch m)
			(cond
				((eq? m 'empty?) empty-queue?)
				((eq? m 'front) front-queue)
				((eq? m 'insert) insert-queue!)
				((eq? m 'delete) delete-queue!)
				((eq? m 'print) print-queue)
				(else (error "unknown" m))))

		dispatch))

(define q (make-queue))
((q 'print))
((q 'insert) 1)
((q 'insert) 2)
((q 'insert) 3)
((q 'print))
((q 'delete))
((q 'print))


;3.23
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue)
	(cons '() '()))

(define (front-queue queue)
	(car (front-ptr queue)))

(define (rear-queue queue)
	(car (rear-ptr queue)))

(define (empty-queue? queue)
	(null? (front-ptr queue)))

(define (front-insert-queue! queue item)
	(let ((new-pair (cons item '())))
		(cond
			((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair))
			(else
				(set-front-ptr! queue 
					(cons item (front-ptr queue)))))))

(define (rear-insert-queue! queue item)
	(let ((new-pair (cons item '())))
		(cond
			((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair))
			(else
				(set-cdr! (rear-ptr queue) new-pair)
				(set-rear-ptr! queue new-pair)))))

(define (front-delete-queue! queue)
	(set-front-ptr! queue (cdr (front-ptr queue))))

(define dq (make-queue))
(front-insert-queue! dq 2)
(front-insert-queue! dq 1)
(rear-insert-queue! dq 3)
(rear-insert-queue! dq 4)
(front-insert-queue! dq 0)
(rear-insert-queue! dq 5)
(print dq)


;表の表現
(define (my-assoc key records)
	(cond
		((null? records) #f)
		((equal? key (caar records)) (car records))
		(else (my-assoc key (cdr records)))))

(define (lookup key table)
	(let ((record (my-assoc key (cdr table))))
		(if record
			(cdr record)
			#f)))

(define (insert! key value table)
	(let ((record (my-assoc key (cdr table))))
		(if record
			(set-cdr! record value)
			(set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
	(list '*table*))


(define (lookup key-1 key-2 table)
	(let ((subtable (my-assoc key-1 (cdr table))))
		(if subtable
			(let ((record (my-assoc key-2 (cdr subtable))))
				(if record
					(cdr record)
					#f)
			#f))))

(define (insert! key-1 key-2 value table)
	(let ((subtable (my-assoc key-1 (cdr table))))
		(if subtable
			(let ((record (my-assoc key-2 (cdr subtable))))
				(if record
					(set-cdr! record value)
					(set-cdr! subtable
						      (cons (cons key-2 value)
						      	    (cdr subtable)))))
			(set-cdr! table
				      (cons (list key-1 (cons key-2 value))
				      	    (cdr table)))))
	'ok)

(define (make-table)
	(let ((local-table (list '*table*)))
		(define (lookup key-1 key-2)
			(let ((subtable (my-assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (my-assoc key-2 (cdr subtable))))
					(if record
						(cdr record)
						#f)
				#f))))
		(define (insert! key-1 key-2 value)
			(let ((subtable (my-assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (my-assoc key-2 (cdr subtable))))
						(if record
							(set-cdr! record value)
							(set-cdr! subtable
						    		  (cons (cons key-2 value)
						      	    	    (cdr subtable)))))
					(set-cdr! local-table
				    		  (cons (list key-1 (cons key-2 value))
				      	    	    (cdr local-table)))))
			'ok)
		(define (dispatch m)
			(cond 
				((eq? m 'lookup-proc) lookup)
				((eq? m 'insert-proc!) insert!)
				(else (error "Unknown operation --TABLE" m))))
		dispatch))


(define operation-table (make-table))

(define get (operation-table 'lookup-proc))
(define set (operation-table 'insert-proc!))


(define (make-table same-key?)
	(let ((local-table (list '*table*)))
		(define (my-assoc key records)
			(cond
				((null? records) #f)
				((same-key? key (caar records)) (car records))
				(else (my-assoc key (cdr records)))))
		(define (lookup key-1 key-2)
			(let ((subtable (my-assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (my-assoc key-2 (cdr subtable))))
					(if record
						(cdr record)
						#f)
				#f))))
		(define (insert! key-1 key-2 value)
			(let ((subtable (my-assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (my-assoc key-2 (cdr subtable))))
						(if record
							(set-cdr! record value)
							(set-cdr! subtable
						    		  (cons (cons key-2 value)
						      	    	    (cdr subtable)))))
					(set-cdr! local-table
				    		  (cons (list key-1 (cons key-2 value))
				      	    	    (cdr local-table)))))
			'ok)
		(define (dispatch m)
			(cond 
				((eq? m 'lookup-proc) lookup)
				((eq? m 'insert-proc!) insert!)
				(else (error "Unknown operation --TABLE" m))))
		dispatch))


(define (lookup key-list table)
	(let ((subtable (my-assoc (car key-list) (cdr table))))
		(if subtable
			(if (null? (cdr key-list))
				(cdr subtable)
				(lookup (cdr key-list) subtable))
			#f)))

(define (insert! key-list value table)
	(let ((subtable (my-assoc (car key-list) (cdr table))))
		(if subtable
			(if (null? (cdr key-list))
				(set-cdr! subtable value)
				(insert! (cdr key-list) value subtable))
			(set-cdr! table
				(cons (if (null? (cdr key-list))
				       	  (cons (car key-list) value)
                  		  (let ((newtable (list (car keylist))))
                      		(insert! (cdr keylist) value newtable)
                      		newtable))
					   (cdr table)))))
	'ok)

;メモ化
;2.27
(define (my-assoc key records)
	(cond
		((null? records) #f)
		((equal? key (caar records)) (car records))
		(else (my-assoc key (cdr records)))))

(define (lookup key table)
	(let ((record (my-assoc key (cdr table))))
		(if record
			(cdr record)
			#f)))

(define (insert! key value table)
	(let ((record (my-assoc key (cdr table))))
		(if record
			(set-cdr! record value)
			(set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
	(list '*table*))

(define (fib n)
	(cond 
		((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
			     (fib (- n 2))))))

(define (memoize f)
	(let ((table (make-table)))
		(lambda (x)
			(let ((previously-computed-result (lookup x table)))
				(or previously-computed-result
					(let ((result (f x)))
						(insert! x result table)
						result))))))

(define memo-fib
	(memoize 
		(lambda (n)
			(cond 
				((= n 0) 0)
				((= n 1) 1)
				(else (+ (memo-fib (- n 1))
					     (memo-fib (- n 2))))))))

(print (memo-fib 10))
