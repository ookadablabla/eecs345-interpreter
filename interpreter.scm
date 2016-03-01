; EECS 345 Class Project 2
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

; Parse and evaluate the file.
(define interpret
  (lambda (filename)
    (call/cc 
      (lambda (return) ; TODO: You're trying to figure out how to forbid BREAKs outside of while loops. Initial BREAK should throw an error.
        (letrec ((loop (lambda (statement state)
                          (if (null? statement) 
                            (return "Reached EOF without a return statement")
                            (loop (restOfExpressions statement) (Mstate (firstExpression statement) state return (lambda (x) (error 'invalidBreak "Break was called outside of a while loop"))))))))
                (loop (parser filename) '((true false) (true false))))))))

; Mstate modifies the state depending on the contents of statement.
(define Mstate
  (lambda (statement state return break)
    (cond 
      ((eq? (operator statement) 'begin) (getInnerScope (Mstate (operand statement) (addLevelOfScope state) return)))
      ((eq? (operator statement) 'return) (return (Mvalue (operand statement) state)))
      ((eq? (operator statement) 'if) (Mstate-if statement state return break))
      ((eq? (operator statement) 'while) 
        (call/cc
          (lambda (new-break)
            (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state return new-break))))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'var) (Mstate-var statement state))
      ((eq? (operator statement) '=) (Mstate-assignment statement state))
      (else (error 'unknown "Encountered an unknown statement")))))

; Mstate-if handles if statements
(define Mstate-if
  (lambda (statement state return break)
    (cond
      ((Mbool (if-condition statement) state) (Mstate (if-statement statement) state return break))
      ((not (null? (else-statement-exists statement))) (Mstate (else-statement statement) state return break))
      (else state))))

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state return break)
    (if (and (eq? (Mbool condition state) 'true) (eq? (lookup 'return state) 'null))
      (Mstate-while condition statement (Mstate statement state) return break))
      state))

; MState-eq handles variable declaration
(define Mstate-var
  (lambda (statement state)
    (cond
      ((stateContains (variable statement) state) (error 'redefining "you can not declare a variable that has already been declared"))
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined state))
      (else (insert (variable statement) (Mvalue (operation statement) state) state)))))

; Mstate-assignment handles variable assignment
(define Mstate-assignment
  (lambda (statement state)
    (insert (variable statement) (Mvalue (operation statement) state) state)))

; Mvalue: Evaluate an expression to determine its value.
(define Mvalue
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((not (list? statement)) (lookup statement state))
      ((eq? (operator statement) '+) (+ (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '-) (if (null? (cddr statement))
                                         (- (Mvalue (operand1 statement) state)) ; unary "-"
                                         (- (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state))))
      ((eq? (operator statement) '*) (* (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '/) (quotient (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      ((eq? (operator statement) '%) (remainder (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
      (else (Mbool statement state)))))
      ;(else (error 'invalidInput "Expression cannot be evaluated to a value")))))

; Mbool: Evaluate a statement for a truth value of #t or #f. 
(define Mbool
  (lambda (statement state)
    (cond 
      ((eq? statement 'true) 'true)
      ((eq? statement 'false) 'false)
      ((eq? (comparator statement) '>) (if (> (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)) 'true 'false))
      ((eq? (comparator statement) '<) (if (< (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)) 'true 'false))
      ((eq? (comparator statement) '>=) (if (>= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)) 'true 'false))
      ((eq? (comparator statement) '<=) (if (<= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)) 'true 'false))
      ((eq? (comparator statement) '==) (if (= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)) 'true 'false))
      ((eq? (comparator statement) '!=) (if (not (= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state))) 'true 'false))
      ((eq? (operator statement) '&&) (if (eq? #t (and (eq? 'true (Mbool (operand1 statement) state)) (eq? 'true (Mbool (operand2 statement) state)))) 'true 'false))
      ((eq? (operator statement) '||) (if (eq? #t (or (eq? 'true (Mbool (operand1 statement) state)) (eq? 'true (Mbool (operand2 statement) state)))) 'true 'false))
      ((eq? (operator statement) '!) (if (eq? #t (not (eq? 'true (Mbool (operand1 statement) state)))) 'true 'false))
      (else (error 'invalidInput "This expression cannot be evaluated to a boolean value")))))

; HELPER METHODS

;lookup gets the value for a given variable
;takes a variable name and the state and returns the value of that variable
(define lookup
  (lambda (var state)
    (lookup-flattened var (cons (flatten (variables state)) (cons (flatten (valuesInState state)) '())))))

(define lookup-flattened
  (lambda (var state)
    (cond
      ((null? (variables state)) (error 'unknown "that variable does not exist"))
      ((eq? (variable1 state) var) (valueOfVar1 state))
      (else (lookup var (cons (restOfVars state) (cons (restOfValues state) '())))))))

;remove removes a variable from the state
; it takes the variable name and the state and removes it from the state
(define replace_var
  (lambda (var value state)
    (cond
      ((null? (variables state)) state)
      ((list? (outerLevelVariables state)) (cons (cons (variables (replace_var var value (cons (outerLevelVariables state) (cons (outerLevelValues state) '()))))
                                                       (cons (variables (replace_var var value (cons (secondLevelVariables state) (cons (secondLevelValues state) '())))) '()))
                                                 (cons (cons (valuesInState (replace_var var value (cons (outerLevelVariables state) (cons (outerLevelValues state) '()))))
                                                       (cons (valuesInState (replace_var var value (cons (secondLevelVariables state) (cons (secondLevelValues state) '())))) '())) '())))
      ((eq? (variable1 state) var) (cons (cons (variable1 state) (restOfVars state)) (cons (cons value (restOfValues state)) '())))
      (else (cons (cons (variable1 state) (variables (replace_var var value (cons (restOfVars state) (cons (restOfValues state) '())))))
                  (cons (cons (valueOfVar1 state) (allValues (replace_var var value (cons (restOfVars state) (cons (restOfValues state) '()))))) '()))))))

;insert inerts a variable into the state, if the value already exists it replaces it
;returns the state with a given variable and value added in
(define insert
  (lambda (var value state)
    (cond
      ((null? state) (cons (cons var '()) (cons (car (cons (cons value state) '())) '())))
      ((null? (variables state)) state)
      ((stateContains var state) (replace_var var value state))
      ((list? (outerLevelVariables state)) (cons (cons (cons var (outerLevelVariables state)) (cons (secondLevelVariables state) '()))
                                                 (cons (cons (cons value (outerLevelValues state)) (cons (secondLevelVariables state) '())) '())))
      (else (cons (cons var (variables state)) (cons (cons value (allValues state)) '()))))))

;stateContains? checks if the variable has already been declared in the state
(define stateContains
  (lambda (var state)
    (stateContains-flattened var (cons (flatten (variables state)) (cons (flatten (valuesInState state)) '())))))

(define stateContains-flattened
  (lambda (var state)
    (cond 
      ((null? (variables state)) #f)
      ((eq? (variable1 state) var) #t)
      (else (stateContains var (cons (restOfVars state) (cons (restOfValues state) '())))))))

;flatten flattens out a list
(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))

;adds a level of scope to the given state
(define addLevelOfScope
  (lambda (state)
    (cons (cons '() (cons (car state) '())) (cons (cons '() (cons (cadr state) '())) '()))))

;remove the outer most level of scope
(define getInnerScope
  (lambda (state)
    (cons (cadar state) (cons (cadadr state) '()))))

; comparator
(define comparator car)

;operator
(define operator car)

; operand
(define operand cadr)

;operand1
(define operand1 cadr)

;operand2
(define operand2 caddr)

;variables in the state
(define variables car)

;values in the state
(define valuesInState cadr)

;outerLevelVariables gets the variables in the outer most scope
(define outerLevelVariables caar)

;outerLevelValues gets the values in the outer most scope
(define outerLevelValues caadr)

;secondLevelVariables gets the variables in the outer most scope
(define secondLevelVariables cadar)

;secondLevelValues gets the values in the outer most scope
(define secondLevelValues cadadr)

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
(define restOfExpressions cdr)

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