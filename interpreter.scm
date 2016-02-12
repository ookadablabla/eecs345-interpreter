; EECS 345 Class Project 1
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (Mevaluate (parser filename) '())))

; MSTATE AND HELPERS
(define Mstate
  (lambda (statement state)
    (cond 
      ((eq? (car statement) 'if) (Mstate-if condition statement state)) ; how to handle else?
      ((eq? (car statement) 'while) (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state))
      ((eq? (car statement) 'var) (Mstate-var statement state))
      ((eq? (car statement) '=) (Mstate-assignment statement statement))
      (else (error 'unknown "Encountered an unknown statement")))))

;Mevaluate
(define Mevaluate ; rename to 'evaluate'? 
  (lambda (statement state)
    (cond
      ((eq? (action statement) 'return) (Mvalue expression statement))
      (else (Mevaluate (cdr statement) (Mstate statement state))))))

;action
(define action caar)

;the expression being returned
(define expression cdar)  

; returns the condition from an "if" statement
(define parse-if-condition
  (lambda (if-statement)
    -1))

; returns the statement from an "if" statement
(define parse-if-statement
  (lambda (if-statement)
    -1))

(define parse-if-else
  (lambda (if-statement)
    -1))

(define parse-while-condition cadr)

(define parse-while-statement caddr)

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
(define Mstate-var
  (lambda (statement state)
    (cond
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined))
      (else (insert (variable statement) (Mvalue (operation statement) state) (remove (variable statement)))))))

; Mstate-assignment handles variable assignment
(define Mstate-assignment
  (lambda (statement state)
    (insert (variable statement) (Mvalue (operation statement) state) (remove (variable statement) state))))

;variable
(define variable cadr)

;third element
(define thirdElement cddr)

;operation
(define operation caddr)

; MVALUE AND HELPERS
(define Mvalue
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((eq? (operator statement) '+) (+ (Mvalue (operand1 statement)) (Mvalue (operand2 statement))))
      ((eq? (operator statement) '-) (- (Mvalue (operand1 statement)) (Mvalue (operand2 statement))))
      ((eq? (operator statement) '*) (* (Mvalue (operand1 statement)) (Mvalue (operand2 statement))))
      ((eq? (operator statement) '/) (quotient (Mvalue (operand1 statement)) (Mvalue (operand2 statement))))
      ((eq? (operator statement) '%) (remainder (Mvalue (operand1 statement)) (Mvalue (operand2 statement))))
      ((eq? (lookup (car statement)) 'undefined) (error 'undefined "the variable is not defined"))
      (else (lookup (car statement))))))

; Evaluate a statement for a truth value of #t or #f. 
(define Mbool
  (lambda (statement state)
    (cond 
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((eq? (car statement) '>) (> (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '<) (< (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '>=) (>= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '<=) (<= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '==) (= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '!=) (not (= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state))))
      ((eq? (comparator statement) '&&) (and (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '||) (or (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '!) (not (Mvalue (operand1 statement) state))) ; almost definitely wrong
      (else (error 'invalidInput "This expression cannot be evaluated to a boolean value")))))

; HELPER METHODS

; comparator
(define comparator car)

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
      ((null? (variables state)) (error 'unkown "that variable does not exist"))
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

;gets the first variable in the state
(define variable1 caar)

;gets the value associated with the first variable in the state
(define valueOfVar1 caadr)

;rest of the variables in the state
(define restOfVars cdar)

;rest of the values in the state
(define restOfValues cdadr)

;get the values in the state
(define allValues cadr)
      


