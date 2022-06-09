(import (r7rs))

;抽象データの多重表現

;複素数の表現
(define (add-complex z1 z2)
	(make-from-real-imag
		(+ (real-part z1) (real-part z2))
		(+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
	(make-from-real-imag
		(- (real-part z1) (real-part z2))
		(- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
	(make-from-mag-ang
		(* (magnitude z1) (magnitude z2))
		(+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
	(make-from-mag-ang
		(/ (magnitude z1) (magnitude z2))
		(- (angle z1) (angle z2))))


(define (real-part z) (car z))

(defin (imag-part z) (cdr z))

(define (magnitude z)
	(sqrt (+ (square (real-part z))
		     (square (imag-part z)))))

(define (angle z)
	(atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))

#|
(define (real-part z) 
	(* (magnitude z) (cos (angle z))))

(defin (imag-part z) 
	(* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y) 
	(cons (sqrt (+ (square x) (square y)))
		  (atan y x)))

(define (make-from-mag-ang r a) (cons r a))
|#


(define (attach-tag type-tag contents)
	(cons type-tag contents))

(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
	(if (pair? datum)
		(cdr datum)
		(error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
	(eq? (type-tag z) 'rectangular))

(define (polar? z)
	(eq? (type-tag z) 'polar))


(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
	(sqrt (+ (square (real-part-rectangular z))
		     (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
	(atan (imag-part-rectangular z)
	      (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y) 
	(attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
	(attach-tag 'rectangular 
		(cons (* r (cos a)) (* r (sin a)))))


(define (real-part-polar z) 
	(* (magnitude-polar z) (cos (angle-polar z))))

(defin (imag-part-polar z) 
	(* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitud-polare z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y) 
	(attach-tag 'polar
		(cons (sqrt (+ (square x) (square y)))
			  (atan y x))))

(define (make-from-mag-ang-polar r a) 
	(attach-tag 'polar (cons r a)))


(define (real-part z)
	(cond
		((rectangular? z)
			(real-part-rectangular (contents z)))
		((polar? z)
			(real-part-polar (contents z)))
		(else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
	(cond
		((rectangular? z)
			(imag-part-rectangular (contents z)))
		((polar? z)
			(imag-part-polar (contents z)))
		(else (error "Unknown type -- imag-PART" z))))

(define (magnitude z)
	(cond
		((rectangular? z)
			(magnitude-rectangular (contents z)))
		((polar? z)
			(magnitude-polar (contents z)))
		(else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
	(cond
		((rectangular? z)
			(angle-rectangular (contents z)))
		((polar? z)
			(angle-polar (contents z)))
		(else (error "Unknown type -- ANGLE" z))))


;汎用的な実装となったが、複素数の表現が二つでなく、数百あるとすると大変
;さらに部品化するデータ主導プログラミングが必要
;データ主導プログラミングは省略


;メッセージパッシング
(define (make-from-real-imag x y)
	(define (dispatch op)
		(cond
			((eq? op 'real-part) x)
			((eq? op 'imag-part) y)
			((eq? op 'magnitude)
				(sqrt (+ (square x) (square y))))
			((eq? op 'angle) (atan y x))
			(else
				(error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
	dispatch)

(define (apply-generic op arg) (arg op))

;2.75
(define (make-from-mag-ang r a)
	(define (dispatch op)
		(cond
			((eq? op 'real-part) (* r (cos a)))
			((eq? op 'imag-part) (* r (sin a)))
			((eq? op 'magnitude) r)
			((eq? op 'angle) a)
			(else
				(error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
	dispatch)

;汎用算術演算
#|
(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
	(define (tag x)
		(attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
		(lambda (x y) (tag (+ x y))))
	(put 'sub '(scheme-number scheme-number)
		(lambda (x y) (tag (- x y))))
	(put 'mul '(scheme-number scheme-number)
		(lambda (x y) (tag (* x y))))
	(put 'div '(scheme-number scheme-number)
		(lambda (x y) (tag (/ x y))))
	(put 'make 'scheme-number
		(lambda (x) (tag x)))
	'done)

(define (make-scheme-number n)
	((get 'make scheme-number) n))
|#

;2.78
(define (type-tag datum)
	(cond 
		((number? datum) 'scheme-number)
     	((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  	(cond 
  		((number? datum) datum)
    	((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag tag-type contents)
  	(if (eq? tag-type 'scheme-number)
  		contents
      	(cons tag-type contents)))
