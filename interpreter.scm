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
      ((eq? (car statement) 'while) (Mstate-while statement state))
      ((eq? (car statement) 'var) (Mstate-var statement state))
    )))

(define Mstate-if)

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state)
    (if (Mbool condition state)
        (Mstate-while condition statement (Mstate-statement statement state)))
    (else state)))

; MState-eq handles variable declaration
(define Mstate-var)

; Mstate-statement takes a statement and the current state, evaluates the 
; statement, and modifies the state as appropriate based on the contents of
; the statement
(define Mstate-statement)

; Mstate-eq handles variable assignment
(define Mstate-eq)

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
