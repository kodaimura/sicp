(import (r7rs))

;非決定性計算
(define (require p)
	(if (not p) (amb)))

;4.35
(define (an-integer-between low high)
	(require (< low high))
	(amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high)))
		(let ((j (an-integer-between i high)))
			(let ((k (an-integer-between j high)))
				(require (= (+ (* i i) (* j j)) (* k k)))
				(list i j k)))))



(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)

;4.40
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 3 4 5)))
    (require (not (= baker cooper)))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= baker fletcher)))
      (require (not (= cooper fletcher)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((miller (amb 1 2 3 4 5)))
        (require (not (= baker miller)))
        (require (not (= cooper miller)))
        (require (not (= fletcher miller)))
        (require (> miller cooper))
        (let ((smith (amb 1 2 3 4 5)))
          (require (not (= baker smith)))
          (require (not (= cooper smith)))
          (require (not (= fletcher smith)))
          (require (not (= miller smith)))
          (require (not 
            (= (abs (- smith fletcher)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))
