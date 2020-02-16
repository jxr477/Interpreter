#lang racket
(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (cond
      ((null? filename) (error "not a valid file name"))
      (else (interpret_helper (parser filename) '())))))

(define interpret_helper
  (lambda (expression state)
    (cond
      ((number? state) state)
      ((boolean? state) state)
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? (car (car expression)) 'return) (Mstate (car expression) state))
      (else (interpret_helper (cdr expression) (Mstate (car expression) state))))))
; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
;
; This is an example of using abstraction to make the code easier to read and maintain
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((eq? 'null expression) (error "vairable value is not assigned"))
      ((eq? #t (exist expression state)) (Mvalue (lookup expression state) state))
      ((eq? #f (pair? expression)) (error "variable is not declared"))
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '< (operator expression)) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '> (operator expression)) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '&& (car expression)) (and (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? '! (car expression)) (not (Mboolean (leftoperand expression) state)))
      ((eq? '|| (car expression)) (or (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      (else (error 'badop "The operator is not known")))))

(define Mboolean
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((boolean? expression) expression)
      ((number? expression) expression)
      ((eq? 'null expression) (error "vairable value is not assigned"))
      ((eq? #t (exist expression state)) (Mvalue (lookup expression state) state))
      ((eq? '&& (car expression)) (and (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? '! (car expression)) (not (Mboolean (leftoperand expression) state)))
      ((eq? '|| (car expression)) (or (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '< (operator expression)) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '> (operator expression)) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      )))


(define if-func
  (lambda (condition stmt1 state [stmt2 '()])
    (cond
      [(eq? (Mboolean condition state) #t) (Mstate stmt1 state)]
      [(and (eq? (Mboolean condition state) #f) (not (eq? '() stmt2))) (Mstate stmt2 state)]
      [else state])))


(define return-func
  (lambda (expression state)
    (cond
      [(null? expression) (error 'parser "parser should have caught this")]
      ((eq? #t (Mvalue expression state)) 'true)
      ((eq? #f (Mvalue expression state)) 'false)
      [else (Mvalue expression state)])))

; The helper methods to define and abstract away the operator, leftoperand, and rightoperand parts of an expression
(define operator
  (lambda (expression)
    (car expression)))

(define leftoperand
  (lambda (expression)
    (cond
      ((null? (cddr expression)) 0)
      ((eq? (cadr expression) 'true) #t)
      ((eq? (cadr expression) 'false) #f)
      (else (cadr expression)))))

(define rightoperand
  (lambda (expression)
    (cond
      ((null? (cddr expression)) (cadr expression))
      ((eq? (caddr expression) 'true) #t)
      ((eq? (caddr expression) 'false) #f)
      (else (caddr expression)))))

(define lookup
  (lambda (var state)
    (cond
      [(null? state) (error "Variable is not declared")]
      [(eq? var (car (car state))) (cadr (car state))]
      [else (lookup var (cdr state))])))

(define exist
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(eq? var (car (car state))) #t]
      [else (exist var (cdr state))])))


(define add
  (lambda (Vname Vvalue state)
    (cond
      ((null? state) (cons (flatten (cons Vname Vvalue)) state))
      (else (cons (flatten (cons Vname Vvalue)) state)))))

(define remove
  (lambda (Vname state)
    (cond
      [(null? state)
       (error "using before declaring")]
      [(eq? Vname (car (car state))) (cdr state)]
      [else cons (car state) (remove Vname (cdr state))])))

(define declare
  (lambda (Vname state [Vvalue 'null])
    (cond
      ((null? Vname) (error "not a viable name"))
      ((eq? #t (exist Vname state)) (error "Variable name already exists"))
      ((eq? Vvalue 'null) (cons (flatten (cons Vname Vvalue)) state))
      (else (cons (flatten (cons Vname (Mvalue Vvalue state))) state)))))

(define assign
  (lambda (Vname Vexpression state)
    (cond
      ((null? state) (error "no variable declared"))
      (else (add Vname (Mvalue Vexpression state) (remove Vname state))))))

(define Mstate
  (lambda (stmt state)
    (cond
      [(and (eq? 'var (car stmt)) (null? (cddr stmt))) (declare (cadr stmt) state)] ;declare
      [(and (eq? 'var (car stmt)) (not (null? (cddr stmt)))) (declare (cadr stmt) state (caddr stmt))]
      [(eq? '= (car stmt)) (assign (cadr stmt) (caddr stmt) state)] ;assign
      [(and (eq? 'if (car stmt)) (null? (cdddr stmt))) (if-func (cadr stmt) (caddr stmt) state)] ;if
      [(and (eq? 'if (car stmt)) (not (null? (cdddr stmt)))) (if-func (cadr stmt) (caddr stmt) state (cadddr stmt))]
      [(eq? 'return (car stmt)) (return-func (cadr stmt) state)] ;return
      [(eq? 'while (car stmt)) (while (cadr stmt) (caddr stmt) state)] ;while
      [else (assign (cadr stmt) (Mvalue stmt state) state)]
      )))


(define while
  (lambda (condition stmt state)
    (cond
      [(eq? (Mboolean condition state) #t) (while condition stmt (Mstate stmt state))]
      [else state]
      )))



