(import (r7rs))

;自己評価式
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;変数
(define (variable? exp) (symbol? exp))

;クォート式
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;述語のテスト
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


;lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;導出された式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

#|
(define (expand-clauses clauses)
  (if (null? clauses)
      'false    ;else句無し
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
|#

#|
(define (eval exp env)
	(cond 
		((self-evaluating? exp) exp)
		((variable? exp) (loolup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
				              (lambda-body exp)
				              env))
		((begin? exp)
			(eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp)
			(apply (eval (operator exp) env)
				     (list-of-values (operands exp) env)))
		(else (error "Unknown expression type -- EVAL" exp))))
|#

(define (apply procedure arguments)
	(cond 
		((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments))
		((compound-procedure? procedure)
			(eval-sequence
				(procedure-body procedure)
				(extend-environment
					(procedure-parameters procedure)
					arguments
					(procedure-environment procedure))))
		(else
			(error "Unknown procedure type -- APPLY" procedure))))


(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;4.1
;右から左
(define (list-of-values exps env)
	(if (no-operands? exps)
		  '()
		  (append (list-of-values (rest-operands exps) env)
		  	      (list (eval (first-operand exps) env)))))

;左から右
(define (list-of-values exps env)
	(if (no-operands? exps)
		  '()
		  (cons (eval (first-operand exps) env)
		  	    (list-of-values (rest-operands exps) env))))


;4.4
(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exps env)
	(define (iter exps)
		(cond 
			((last-exp? exps) (eval (first-exp exps) env))
			((eval (first-exp exps) env) (iter (cdr exps)))
			(else #f)))
	(iter (cdr exps)))

(define (eval-or exps env)
	(define (iter exps)
		(cond 
			((last-exp? exps) (eval (first-exp exps) env))
			((eval (first-exp exps) env) #t)
			(else (iter (cdr exps)))))
	(iter (cdr exps)))


;4.5
(define (expand-clauses clauses)
  (if (null? clauses)
      'false    ;else句無し
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (eq? (cadr first) '=>)
        	  (make-if (cond-predicate first)
        	  	       (list (caddr first) (cond-predicate first))
        	  	       (expand-clauses rest)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;4.6
(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
	(let ((bindings (cadr exp)) (body (cddr exp)))
  	(cons (make-lambda (map car bindings) body)
        	(map cadr bindings))))


;4.7
(define (make-let bindings body)
	(let->combination (list 'let bindings body)))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
	(define (iter bindings body)
		(if (null? bindings)
			  (sequence->exp body)
			  (make-let (list (car bindings))
			  	        (iter (cdr bindings) body))))
	(iter (cadr exp) (cddr exp)))


;4.8
(define (let->combination exp)
  (if (symbol? (cadr exp))
    (let ((tag (cadr exp)) 
    	    (bindings (caddr exp)) 
    	    (body (cdddr exp)))
      (list (list 'lambda '()
              (cons 'define (cons (cons tag (map car bindings)) body))
              	(cons tag (map cadr bindings)))))
  	(let ((bindings (cadr exp)) (body (cddr exp)))
   		(cons (make-lambda (map car bindings) body)
    		(map cadr bindings)))))


(define (eval exp env)
	(cond 
		((self-evaluating? exp) exp)
		((variable? exp) (loolup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
				              (lambda-body exp)
				              env))
		((begin? exp)
			(eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((let? exp) (eval (let->combination exp) env))
		((let*? exp) (eval (let*->nested-lets exp) env))
		((application? exp)
			(apply (eval (operator exp) env)
				     (list-of-values (operands exp) env)))
		(else (error "Unknown expression type -- EVAL" exp))))


;手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;環境に対する操作

;環境の外側の環境
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

#|
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

|#

;4.12
(define (env-loop var env proc-find proc-null error-msg) 
  (define (scan vars vals)
    (cond ((null? vars) (proc-null env))
          ((eq? var (car vars))
            (proc-find vals))
          (else (scan (cdr vars) (cdr vals))))
  (if (eq? env the-empty-environment)
      (error error-msg var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame))))))

#|
(define (lookup-variable-value var env)
  (env-loop var 
  	        env
  	        car
  	        (lookup-variable-value var (enclosing-environment env)
  	        "Unbound variable"))
|#

(define (set-variable-value! var val env)
  (env-loop var
  	        env
  	        (lambda (vals) (set-car! vals val))
  	        (set-variable-value! var val (enclosing-environment env))
  	        "Unbound variable -- SET!"))


(define (define-variable! var val env)
	(let ((frame (first-frame env)))
  	(env-loop var
  						(list frame)
            	(lambda (vals) (set-car! vals val)) 
            	(lambda (env) (add-binding-to-frame! var val frame))
              "")))

;4.13
(define (unbind! var env)
	(let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars))
            ((eq? var (car vars)) (set-car! vals '()))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;評価器をプログラムとして走らせる

(define primitive-procedures
  (list (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '< <)
        (list '= =)
        (list '> >)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'eq? eq?)
        (list 'list list)
        (list 'not not)
        (list 'null? null?)
        ))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))


;4.16
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
             	   (error "Unassigned variable" var)
             	   (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (scan-out-of-defines proc-body)
	(let ((lets '())
		    (sets '()))
		(define (scan body)
			(cond
				((tagged-list? (car body) 'define)
					(set! lets (cons (list (cadar body) '*unassigned*) lets))
					(set! sets (cons (list 'set! (cadar body) (caddar body)) sets))
					(scan (cdr body)))
				(else (car body))))
		(let ((body (scan (lambda-body proc-body))))
			(list (car proc-body) (cadr proc-body)
				(list 'let lets)
					(cons 'begin (append sets (list body)))))))


(print (scan-out-of-defines
	'(lambda (x)
		(define y 2)
		(define z 3)
		(print (+ x y x)))))
		

(define (make-procedure parameters body env)
  (list 'procedure parameters 
  	     (scan-out-of-defines body) env))


;構文解析を実行から分離
(define (eval exp env)
  ((analyze exp) env))

;4.22
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))
