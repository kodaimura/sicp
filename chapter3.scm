(import (r7rs))
(import (chicken random))

(define random pseudo-random-integer)

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


(define random-init 1)

;ランダムではないが、とりあえずで
(define (rand-update x)
	(+ x 2))

(define rand
	(let ((x random-init))
		(lambda ()
			(set! x (rand-update x))
			x)))

(define (estimate-pi trials)
	(sqrt (/ 6 (monte-calro trials cesaro-test))))

(define (cesaro-test)
	(= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond
			((= trials-remaining 0)
				(/ trials-passed trials))
			((experiment)
				(iter (- trials-remaining 1) (+ trials-passed 1)))
			(else
				(iter (- trials-remaining 1) trials-passed))))
	(iter trials 0))

;3.5
(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))))

(define (area-of-circle P x1 x2 y1 y2 trials)
	(define (test)
		(let ((x (random-in-range x1 x2))
			  (y (random-in-range y1 y2)))
			(P x y)))
	(let ((ratio (monte-carlo trials test)))
		(* (- x2 x1) (- y2 y1) ratio)))

(print (area-of-circle 
			(lambda (x y) (<= (+ (square (- x 5)) (square (- y 7))) 9))
			2 8 4 10
			1000))

;3.6
(define rand
	(let ((x 0))
		(lambda (op)
			(cond
				((eq? op 'generate) 
					(begin (set! x (rand-update x)) x))
				((eq? op 'reset) 
					(lambda (new-value) (set! x new-value) x))
				(else (error "Unknown request -- RAND" op))))))

(print (rand 'generate))
(print (rand 'generate))
(print ((rand 'reset) 10))
(print (rand 'generate))


;3.7
(define (make-account balance password)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)))
	(define (error-message x) "Incorrect password")
	(define (check-password p) (eq? p password))
	(define (dispatch p m)
		(cond
			((eq? m 'joint) (check-password p))
			((not (check-password p)) error-message)
			((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request -- MAKE-ACCOUNT" m))))
	dispatch)

(define (make-joint acc acc-pass joint-pass)
	(define (joint p m)
		(if (eq? joint-pass p)
			(acc acc-pass m)
			(acc #f m)))
	(if (acc acc-pass 'joint)
		joint
		"Incorrect joint password"))

(define a1 (make-account 100 'accpass))
(define a1joint1 (make-joint a1 'accpass 'jointpass))

(print (make-joint a1 'accpa 'jointpass2))
(print ((a1 'accpass 'deposit) 50))
(print ((a1joint1 'jointpass 'withdraw) 30))
(print ((a1joint1 'accpa 'withdraw) 30))


;3.8
(define f
	(let ((x 0))
		(lambda (n)
			(if (= x 0) (begin (set! x 1) n) 0))))

(print (+ (f 1) (f 0)))
(print (+ (f 0) (f 1)))