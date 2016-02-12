; EECS 345 Class Project 1
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
  	; Feed file into parser
  	; Evaluate the parse tree returned by parser
  	; Return the appropriate value
  ))

; MSTATE AND HELPERS
(define Mstate
  (lambda (statement state)
    (cond 
      ((eq? (car statement) 'if) (Mstate-if statement state))
      ((eq? (car statement) 'while) (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state))
      ((eq? (car statement) 'var) (Mstate-var statement state))
      ((eq? (car statement) '=) (Mstate-assignment statement statement))
      ; todo: handle return statements
    )))

; returns the condition from an "if" statement
(define parse-if-condition
  (lambda (if-statement)
    ()))

; returns the statement from an "if" statement
(define parse-if-statement
  (lambda (if-statement)
    ))

(define parse-if-else
  (lambda (if-statement)))

; (parse-while-statement '(while (condition) (statement))) returns condition
(define parse-while-condition
  (lambda (while-statement)
    (cadr while-statement)))

; (parse-while-statement '(while (condition) (statement))) returns statement
(define parse-while-statement
  (lambda (while-statement)
    (caddr while-statement)))

(define Mstate-if
  (lambda (condition statement state)
    ))

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state)
    (if (Mbool condition state)
        (Mstate-while condition statement (Mstate statement state)))
    (else state)))

; MState-eq handles variable declaration
(define Mstate-var)

; Mstate-eq handles variable assignment
(define Mstate-assignment)

; MVALUE AND HELPERS
(define Mvalue
  (lambda (l)
    (cond
      ((number? l) l)
      ((eq? (operator l) '+) (+ (Mvalue (operand1 l)) (Mvalue (operand2 l))))
      ((eq? (operator l) '-) (- (Mvalue (operand1 l)) (Mvalue (operand2 l))))
      ((eq? (operator l) '*) (* (Mvalue (operand1 l)) (Mvalue (operand2 l))))
      ((eq? (operator l) '/) (quotient (Mvalue (operand1 l)) (Mvalue (operand2 l))))
      ((eq? (operator l) '%) (remainder (Mvalue (operand1 l)) (Mvalue (operand2 l))))
      (else (error 'unknown "unknown expression"))))) ;it should never get here

(define Mbool
  (lambda (statement) (
    )))

; HELPER METHODS

;operator
(define operator car)

;operand1
(define operand1 cadr)

;operand2
(define operand2 caddr)

;lookup gets the value for a given variable
;takes a variable name and the state and returns the value of that variable
(define lookup
  (lambda (var s)
    (cond
      ((null? (car s)) 'error)
      ((eq? (caar s) var) (caadr s))
      (else (lookup var (cons (cdar s) (cons (cdadr s) '())))))))
