(import (r7rs))

;balanceはどこからでもアクセスできてしまう
(define balance 100)

(define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			balance)
		"Insufficient funds"))

(print balance)

(withdraw 25)

(print balance)

;withdrawだけがbalanceにアクセスできるようにする
(define (make-withdraw balance)
	(lambda (amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds")))

(define w1 (make-withdraw 100))

(print (w1 10))


(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)))
	(define (dispatch m)
		(cond
			((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request -- MAKE-ACCOUNT" m))))
	dispatch)

(define acc (make-account 100))

(print ((acc 'withdraw) 50))

(print ((acc 'deposit) 60))


;3.1
(define (make-accumulator x)
	(lambda (n)
		(begin (set! x (+ x n)) x)))

(define A (make-accumulator 5))

(print (A 10))

(print (A 10))


;3.2
(define (make-monitored f)
	(define mf 0)
	(lambda (x)
		(cond 
			((eq? x 'how-many-calls?) mf)
			((eq? x 'reset-count) (set! mf 0))
			(else (begin (set! mf (+ mf 1)) (f x))))))

(define s (make-monitored sqrt))

(print (s 100))
(print (s 25))
(print (s 'how-many-calls?))
(print (s 'reset-count))
(print (s 'how-many-calls?))


;3.3
(define (make-account balance password)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)))
	(define (error-message x) "Incorrect password")
	(define (dispatch p m)
		(cond
			((not (eq? p password)) error-message)
			((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request -- MAKE-ACCOUNT" m))))
	dispatch)


(define acc2 (make-account 100 'pass))

(print ((acc2 'pass 'withdraw) 50))

(print ((acc2 'pa 'deposit) 60))


;3.4
(define (make-account balance password)
	(define ipc 0)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)))
	(define (error-message x) "Incorrect password")
	(define (call-the-cops x) "call-the-cops")
	(define (dispatch p m)
		(if (not (eq? p password))
			(begin (set! ipc (+ ipc 1))
				   (if (= ipc 7) call-the-cops error-message))
			(begin (set! ipc 0)
				   (cond
				   	((eq? m 'withdraw) withdraw)
					((eq? m 'deposit) deposit)
					(else (error "Unknown request -- MAKE-ACCOUNT" m))))))
	dispatch)

(define acc3 (make-account 100 'pass))

(print ((acc3 'pass 'withdraw) 50))

(print ((acc3 'pa 'deposit) 60))
(print ((acc3 'pa 'deposit) 60))
(print ((acc3 'pa 'deposit) 60))
(print ((acc3 'pa 'deposit) 60))
(print ((acc3 'pa 'deposit) 60))
(print ((acc3 'pa 'deposit) 60))
(print ((acc3 'pa 'deposit) 60))
