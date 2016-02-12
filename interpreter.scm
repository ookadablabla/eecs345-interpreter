; EECS 345 Class Project 1
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    -1
  	; Feed file into parser
  	; Evaluate the parse tree returned by parser
  	; Return the appropriate value
  )

; MSTATE AND HELPERS
(define Mstate
  (lambda (statement state)
    (cond 
      ((eq? (car statement) 'if) (Mstate-if condition statement state)) ; how to handle else?
      ((eq? (car statement) 'while) (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state))
      ((eq? (car statement) 'var) (Mstate-var statement state))
      ((eq? (car statement) '=) (Mstate-assignment statement statement))
      (else (error 'unknown "Encountered an unknown statement"))
      ; todo: handle return statements
    )))

; returns the condition from an "if" statement
(define parse-if-condition
  (lambda (if-statement)
    -1))

; returns the statement from an "if" statement
(define parse-if-statement
  (lambda (if-statement)
    -1
    ))

(define parse-if-else
  (lambda (if-statement)
    -1
    ))

; (parse-while-statement '(while (condition) (statement))) returns condition
(define parse-while-condition
  (lambda (while-statement)
    (cadr while-statement)))

; (parse-while-statement '(while (condition) (statement))) returns statement
(define parse-while-statement
  (lambda (while-statement)
    (caddr while-statement)))

; Mstate-if handles if statementes
; TODO: How to handle else clause?
(define Mstate-if
  (lambda (condition statement state)
    (if (Mbool condition state)
      (Mstate statement state))))

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state)
    (if (Mbool condition state)
        (Mstate-while condition statement (Mstate statement state)))
    (else state)))

; MState-eq handles variable declaration
(define Mstate-var -1)

; Mstate-assignment handles variable assignment
(define Mstate-assignment -1)

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

; Evaluate a statement for a truth value of #t or #f. 
(define Mbool
  (lambda (statement state)
    (cond 
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((list? statement) (Mstate statement state)) ; Is this right? 
      (else (error 'conditionInvalid "Could not evaluate condition"))
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
      


