; EECS 345 Class Project 1
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '((return) (null)) )))

;evaluate
(define evaluate
  (lambda (statement state)
    (cond
      ((null? statement) (lookup 'return state))
      ((eq? (action statement) 'return) (lookup 'return (Mstate (firstExpression statement) (remove_var 'return state))))
      (else (evaluate (resOfExpressions statement) (Mstate (firstExpression statement) state))))))

; MSTATE AND HELPERS
(define Mstate
  (lambda (statement state)
    (cond 
      ((eq? (operator statement) 'return) (insert 'return (Mvalue (operand1 statement) state) (remove_var 'return state)))
      ((eq? (operator statement) 'if) (Mstate-if statement state))
      ((eq? (operator statement) 'while) (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state))
      ((eq? (operator statement) 'var) (Mstate-var statement state))
      ((eq? (operator statement) '=) (Mstate-assignment statement state))
      (else (error 'unknown "Encountered an unknown statement")))))

;How to handle else clause?
(define Mstate-if
  (lambda (statement state)
    (cond
      ((Mbool (if-condition statement) state) (Mstate (if-statement statement) state))
      ((not (null? (else-statement-exists statement))) (Mstate (else-statement statement) state))
    (else state))))

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state)
    (if (and (Mbool condition state) (not (eq? (lookup 'return state) 'null)))
      (Mstate-while condition statement (Mstate statement state)))
      state))

; MState-eq handles variable declaration
(define Mstate-var
  (lambda (statement state)
    (cond
      ((not (stateContains (variable statement) state)) (error 'redefining "you can not declare a variable that has already been declared"))
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined state))
      (else (insert (variable statement) (Mvalue (operation statement) state) (remove_var (variable statement) state))))))

; Mstate-assignment handles variable assignment
(define Mstate-assignment
  (lambda (statement state)
    (insert (variable statement) (Mvalue (operation statement) state) (remove_var (variable statement) state))))

; Mvalue: Evaluate an expression to determine its value.
(define Mvalue
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((not (list? statement)) (lookup statement state))
      ((eq? (operator statement) '+) (+ (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '-) (- (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '*) (* (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '/) (quotient (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '%) (remainder (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      (else (error 'invalidInput "Expression cannot be evaluated to a value")))))

; Mbool: Evaluate a statement for a truth value of #t or #f. 
(define Mbool
  (lambda (statement state)
    (cond 
      ((eq? statement 'true) 'true)
      ((eq? statement 'false) 'false)
      ((eq? (car statement) '>) (> (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '<) (< (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '>=) (>= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '<=) (<= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '==) (= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (comparator statement) '!=) (not (= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state))))
      ((eq? (comparator statement) '&&) (eq? 'true (and (Mbool (operand1 statement) state) (Mbool (operand2 statement) state))))
      ((eq? (comparator statement) '||) (eq? 'true (or (Mbool (operand1 statement) state) (Mbool (operand2 statement) state))))
      ((eq? (comparator statement) '!) (eq? 'true (not (Mbool (operand1 statement) state))))
      (else (error 'invalidInput "This expression cannot be evaluated to a boolean value")))))

; HELPER METHODS

;lookup gets the value for a given variable
;takes a variable name and the state and returns the value of that variable
(define lookup
  (lambda (var state)
    (cond
      ((null? (variables state)) (error 'unknown "that variable does not exist"))
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

;stateContains? checks if the variable has already been declared in the state
(define stateContains
  (lambda (var state)
    (cond 
      ((null? (variables state)) #f)
      ((eq? (variable1 state) var) #t)
      (else (stateContains var (cons (restOfVars state) (cons (restOfValues state) '())))))))

; comparator
(define comparator car)

;operator
(define operator car)

;operand1
(define operand1 cadr)

;operand2
(define operand2 caddr)

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

;the expression in the stat of the program
(define firstExpression car)

;the rest of the expressions in the programs
(define resOfExpressions cdr)

;action
(define action caar)

;the expression being returned
(define expression cdar)  

(define parse-while-condition cadr)

(define parse-while-statement caddr)

(define else-statement-exists cdddr)

(define if-condition cadr)

(define if-statement caddr)

(define else-statement cadddr)

;variable
(define variable cadr)

;third element
(define thirdElement cddr)

;operation
(define operation caddr)