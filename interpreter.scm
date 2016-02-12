; EECS 345 Class Project 1
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

;(define interpret
;  (lambda (filename)
  	; Feed file into parser
  	; Evaluate the parse tree returned by parser
  	; Return the appropriate value
;  )

; MSTATE AND HELPERS
(define Mstate
  (lambda (statement state)
    (cond 
      ((eq? (car statement) 'if) (Mstate-if statement state))
      ((eq? (car statement) 'while) (Mstate-while statement state))
      ((eq? (car statement) 'var) (Mstate-var statement state))
    )))

;(define Mstate-if)

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state)
    (if (Mbool condition state)
        (Mstate-while condition statement (Mstate-statement statement state)))
    (else state)))

; MState-eq handles variable declaration
;(define Mstate-var)

; Mstate-statement takes a statement and the current state, evaluates the 
; statement, and modifies the state as appropriate based on the contents of
; the statement
;(define Mstate-statement)

; Mstate-eq handles variable assignment
;(define Mstate-eq)

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
  (lambda (var state)
    (cond
      ((null? (variables state)) 'error)
      ((eq? (variable1 state) var) (valueOfVar1 state))
      (else (lookup var (cons (restOfVars state) (cons (restOfValues state) '())))))))

;remove removes a variable from the state
; it takes the variable name and the state and removes it from the state
(define remove_var
  (lambda (var state)
    (cond
      ((null? (variables state)) state)
      ((eq? (variable1 state) var) (cons (restOfVars state) (cons (restOfValues state) '())))
      (else (cons (cons (variable1 state) (variables (remove_var var (cons (restOfVars state) (cons (restOfValues state) '())))))
                  (cons (cons (valueOfVar1 state) (allValues (remove_var var (cons (restOfVars state) (cons (restOfValues state) '()))))) '()))))))

;insert inerts a variable into the state
;returns the state with a given variable and value added in
(define insert
  (lambda (var value state)
    (cond
      ((null? state) (cons (cons var '()) (cons (car (cons (cons value state) '())) '())))
      (else (cons (cons var (variables state)) (cons (cons value (allValues state)) '()))))))

;helpers for lookup, remove, and insert

;variables in the state
(define variables car)

;gets the first variabble in the state
(define variable1 caar)

;gets the value associated with the first variable in the state
(define valueOfVar1 caadr)

;rest of the varaibles in the state
(define restOfVars cdar)

;rest of the values in the state
(define restOfValues cdadr)

;get the values in the state
(define allValues cadr)
      


