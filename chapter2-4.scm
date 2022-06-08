(import (r7rs))

;抽象データの多重表現

;複素数の表現
(define (add-complex z1 z2)
	(make-from-real-image
		(+ (real-part z1) (real-part z2))
		(+ (image-part z1) (image-part z2))))

(define (sub-complex z1 z2)
	(make-from-real-image
		(- (real-part z1) (real-part z2))
		(- (image-part z1) (image-part z2))))

(define (mul-complex z1 z2)
	(make-from-mag-ang
		(* (magnitude z1) (magnitude z2))
		(+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
	(make-from-mag-ang
		(/ (magnitude z1) (magnitude z2))
		(- (angle z1) (angle z2))))


(define (real-part z) (car z))

(defin (image-part z) (cdr z))

(define (magnitude z)
	(sqrt (+ (square (real-part z))
		     (square (image-part z)))))

(define (angle z)
	(atan (image-part z) (real-part z)))

(define (make-from-real-image x y) (cons x y))

(define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))

#|
(define (real-part z) 
	(* (magnitude z) (cos (angle z))))

(defin (image-part z) 
	(* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-image x y) 
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

(define (image-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
	(sqrt (+ (square (real-part-rectangular z))
		     (square (image-part-rectangular z)))))

(define (angle-rectangular z)
	(atan (image-part-rectangular z)
	      (real-part-rectangular z)))

(define (make-from-real-image-rectangular x y) 
	(attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
	(attach-tag 'rectangular 
		(cons (* r (cos a)) (* r (sin a)))))


(define (real-part-polar z) 
	(* (magnitude-polar z) (cos (angle-polar z))))

(defin (image-part-polar z) 
	(* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitud-polare z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-image-polar x y) 
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

(define (image-part z)
	(cond
		((rectangular? z)
			(image-part-rectangular (contents z)))
		((polar? z)
			(image-part-polar (contents z)))
		(else (error "Unknown type -- IMAGE-PART" z))))

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