; EECS 345 Class Project 2
; James Hochadel and Andrew Marmorstein
;
; This code was restructured using solution2.scm from Blackboard to better abstract certain
; functions and generally clean up Mstate.
(load "simpleParser.scm")

; Parse and begin evaluation of the file.
(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (let ((begin-interpret (lambda (statement state) (do-interpret statement state return default-break default-continue default-throw))))
             (begin-interpret (parser filename) initial-state))))))

; do-interpret recursively evaluates statements and modifies the state appropriately
; based on their contents.
(define do-interpret
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (do-interpret (restOfExpressions statement)
                    (Mstate (firstExpression statement) state return break continue throw)
                    return break continue throw))))

(define initial-state '((true false) (true false)))
(define default-break (lambda (s) (error 'invalidBreak "Break was called outside of a while loop")))
(define default-continue (lambda (s) (error 'invalidContinue "Continue was called outside of a while loop")))
(define default-throw (lambda (e s) (error 'uncaughtException "An exception was thrown but not caught")))

; Mstate modifies the state depending on the contents of statement.
(define Mstate
  (lambda (statement state return break continue throw)
    (cond
      ((eq? (operator statement) '=) (Mstate-assignment statement state))
      ((eq? (operator statement) 'begin) (Mstate-begin (cdr statement) state return break continue throw))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'if) (Mstate-if statement state return break continue throw))
      ((eq? (operator statement) 'return) (return (Mvalue (operand statement) state)))
      ((eq? (operator statement) 'throw) (throw (exception statement) state))
      ((eq? (operator statement) 'try) (Mstate-tcf statement state return break continue throw))
      ((eq? (operator statement) 'var) (Mstate-var statement state))
      ((eq? (operator statement) 'function) (Mstate-func statement state))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state return new-break continue throw))))
      (else (error 'unknown "Encountered an unknown statement")))))

(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

; Modify the state based on a try-catch-finally block.
(define Mstate-tcf
  (lambda (statement state return break continue throw)
    (call/cc
      (lambda (catch-continuation)
        (letrec ((finally (lambda (s)
                  (if (pair? (finally-stmt statement))
                      (Mstate-begin (finally-body statement) s return break continue throw)
                      s)))
                (try (lambda (new-throw)
                  ; if this try block is accompanied by a catch block, pass a continuation that
                  ; jumps us to it when we encounter a throw. Otherwise, pass whatever throw continuation
                  ; we were passed when we entered this try block.
                  (if (pair? (catch-block statement))
                    (finally (Mstate-begin (try-body statement) state return break continue new-throw))
                    (finally (Mstate-begin (try-body statement) state return break continue throw)))))
                (catch (lambda (e s)
                  (finally (Mstate-begin (catch-body statement) (insert (catch-err statement) e s) return break continue throw)))))
                (try (lambda (e s) (catch-continuation (catch e s)))))))))

(define catch-block caddr)
(define catch-body (lambda (v) (caddr (caddr v))))
(define try-body cadr)
(define catch-block caddr)
(define catch-err (lambda (v) (car (cadr (caddr v)))))
(define finally-body (lambda (v) (cadr (car (cdddr v)))))

; Whenever entering a block of code with curly braces, this function should be called to evaluate
; the contents of the block inside a new layer of scope.
(define Mstate-begin
  (lambda (statement state return break continue throw)
    (getInnerScope (do-interpret statement
                                 (addLevelOfScope state)
                                 return
                                 (lambda (s) (break (getInnerScope s)))
                                 (lambda (s) (continue (getInnerScope s)))
                                 (lambda (e s) (throw e (getInnerScope s)))))))

; Mstate-if handles if statements
(define Mstate-if
  (lambda (statement state return break continue throw)
    (cond
      ((eq? 'true (Mbool (if-condition statement) state)) (Mstate (if-statement statement) state return break continue throw))
      ((not (null? (else-statement-exists statement))) (Mstate (else-statement statement) state return break continue throw))
      (else state))))

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state return break continue throw)
    (if (eq? 'true (Mbool condition state))
      (Mstate-while condition
                    statement
                    (call/cc
                      (lambda (new-continue)
                        (Mstate statement state return break new-continue throw)))
                    return
                    break
                    continue
                    throw)
      state)))

; MState-var handles variable declaration
(define Mstate-var
  (lambda (statement state)
    (cond
      ((stateContains (variable statement) state) (error 'redefining (format "Variable ~a has already been declared" (variable statement))))
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined state))
      (else (insert (variable statement) (Mvalue (operation statement) state) state)))))

;Mstate-func handles function declorations
(define Mstate-func
  (lambda (statement state)
    (cond
      ((stateContains (funcName statement) state) (error 'redefining (format "function ~a has already been declared" (funcName statement))))
      (else (insert (funcName statement) (createClosure (getParams statement) (getBody statement)) state)))))

;helper methods for Mstate-func
(define funcName cadr)

(define getParams caddr)

(define getBody cadddr)

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

; Mbool: Evaluate a statement for a truth value of true or false.
(define Mbool
  (lambda (statement state)
    (cond
      ((not (list? statement)) (Mvalue statement state))
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
      ((null? (variables state)) (error 'unknown (format "Variable ~a does not exist" var)))
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
                                                 (cons (cons (cons value (outerLevelValues state)) (cons (secondLevelValues state) '())) '())))
      (else (cons (cons var (variables state)) (cons (cons value (allValues state)) '()))))))

;createClosure creates a closure functon that will be added to the state
;the thirsd part of the cosure is the framework for the environment
(define createClosure
  (lambda (params body)
    (cons params (cons body '(((()())(()())))))))

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

;gets the code inside the braces
(define insideBraces cdr)

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

(define insideBraces cdr)

(define exception cadr)

;Helpers for sending information to Mstate-try
(define try cadr)

(define catch caddr)


(define finally cadddr)

(define finallyExpressions cadr)
