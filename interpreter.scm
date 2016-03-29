; EECS 345 Class Project 2
; James Hochadel and Andrew Marmorstein
(load "functionParser.scm")

; Parse and evaluate the file.
(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (letrec ((loop (lambda (statement state)
                          (if (null? statement)
                            (return "Reached EOF without a return statement")
                            (loop (restOfExpressions statement) (Mstate (firstExpression statement)
                                                                        state
                                                                        return
                                                                        (lambda (s) (error 'invalidBreak "Break was called outside of a while loop"))
                                                                        (lambda (s) (error 'invalidContinue "Continue was called outside of a while loop"))
                                                                        (lambda (e s) (error 'uncaughtException "An exception was thrown but not caught"))))))))
                (loop (parser filename) '(((true false) (true false)))))))))

; Mstate modifies the state depending on the contents of statement.
(define Mstate
  (lambda (statement state return break continue throw)
    (cond
      ((eq? (operator statement) '=) (Mstate-assignment statement state))
      ((eq? (operator statement) 'begin) (getInnerScope (Mstate-begin (insideBraces statement)
                                                                      (addLevelOfScope state)
                                                                      return
                                                                      (lambda (s) (break (getInnerScope s)))
                                                                      (lambda (s) (continue (getInnerScope s)))
                                                                      (lambda (e s) (throw e (getInnerScope s))))))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'if) (Mstate-if statement state return break continue throw))
      ((eq? (operator statement) 'return) (return (Mvalue (operand statement) state)))
      ((eq? (operator statement) 'throw) (throw (exception statement) state))
      ((eq? (operator statement) 'try)
        (if (null? (catch statement))
          (Mstate-finally (finally statement)
                          (addLevelOfScope (Mstate-try (try statement) (addLevelOfScope state) return (lambda (s) (break (getInnerScope s)))
                                                                                     (lambda (s) (continue (getInnerScope s)))
                                                                                     (lambda (e s) (throw e (getInnerScope s)))))
                          return
                          (lambda (s) (break (getInnerScope s)))
                          (lambda (s) (continue (getInnerScope s)))
                          (lambda (e s) (throw e (getInnerScope s))))
          (Mstate-finally (finally statement)
                          (addLevelOfScope
                            (call/cc
                              (lambda (new-throw)
                                (Mstate-try (try statement) (addLevelOfScope state) return (lambda (s) (break (getInnerScope s)))
                                                                                           (lambda (s) (continue (getInnerScope s)))
                                                                                           (lambda (e s) (new-throw (Mstate-catch (catch-body (catch statement))
                                                                                                                                  (insert (caadr (catch statement)) e (addLevelOfScope (getInnerScope s)))
                                                                                                                                  return
                                                                                                                                  (lambda (s) (break (getInnerScope s)))
                                                                                                                                  (lambda (s) (continue (getInnerScope s)))
                                                                                                                                  (lambda (e s) (throw e (getInnerScope s))))))))))
                          return
                          (lambda (s) (break (getInnerScope s)))
                          (lambda (s) (continue (getInnerScope s)))
                          (lambda (e s) (throw e (getInnerScope s))))))
      ((eq? (operator statement) 'var) (Mstate-var statement state))
      ((eq? (operator statement) 'function) (Mstate-func statement state))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state return new-break continue throw))))
      (else (error 'unknown "Encountered an unknown statement")))))

;Mstate-try handles try blocks
(define Mstate-try
  (lambda (try state return break continue throw)
    (cond
      ((null? try) (getInnerScope state))
      (else (Mstate-try (restOfExpressions try) (Mstate (firstExpression try) state return break continue throw) return break continue throw)))))

; Mstate-catch handles catch statements
(define Mstate-catch
  (lambda (catch state return break continue throw)
    (if (null? catch)
      (getInnerScope state)
      (Mstate-catch (restOfExpressions catch) (Mstate (firstExpression catch) state return break continue throw) return break continue throw))))

;Mstate-finally handles finally blocks
(define Mstate-finally
  (lambda (finally state return break continue throw)
    (cond
      ((null? finally) (getInnerScope state))
      ((eq? (operator finally) 'finally) (Mstate-finally (finallyExpressions finally) state return break continue throw))
      (else (Mstate-finally (restOfExpressions finally) (Mstate (firstExpression finally) state return break continue throw) return break continue throw)))))

; Mstate-begin handles begin statements
(define Mstate-begin
  (lambda (statement state return break continue throw)
    (cond
      ((null? statement) state)
      (else (Mstate-begin (restOfExpressions statement) (Mstate (firstExpression statement) state return break continue throw) return break continue throw)))))

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
                    throw))
      state))

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

(define lookup
  (lambda (var state)
    (cond
      ((null? state) (error 'unknown (format "Variable ~a does not exist" var)))
      ((varsContain var (variables state)) (lookupVal var (currentLayer state)))
      (else (lookup var (nextLayers state))))))

(define lookupVal
  (lambda (var state)
    (cond
      ((eq? (variable1 state) var) (valueOfVar1 state))
      (else (lookupVal var (cons (restOfVars state) (cons (restOfValues state) '())))))))


;helpers for lookup
(define nextLayers cdr)

(define currentLayer car)

(define variableList caar)

;getEnvirontment gets the environment within which a function call has access
;assumes funcall is of format (funcall 'method name' variable1 variable2 ... variableN)
(define getEnvironmentFromFuncall
  (lambda (funcall state)
    (getEnvironment (name funcall) (getParamsFromState (name funcall) state) (paramValues funcall) state)))

(define getEnvironment
  (lambda (funName funParams funParamValues state)
    (cons (getLocal funParams funParamValues state) (cons (getGlobal funName state))))) 
  
;getGlobal gets the global variables for the environment
(define getGlobal
  (lambda (funName state)
    (cond
      ((varsContain funName (variables state)) state)
      (else (getGlobal funName (nextLayers state))))))

;getLocal get all of the local variable for the function which will be the parameters
(define getLocal
  (lambda (funParams paramValues state)
    (getLocalWithFormat funParams paramValues state '(()()))))

(define getLocalWithFormat
  (lambda (funParams paramValues state localState)
    (cond
      ((null? funParams) localState)
      (else (getLocalWithFormat (restOfParams funParams) (restOfParamValues paramValues) state (currentLayer (insert (currentParam funParams) (Mvalue (currentParamValue paramValues) state) (cons localState '()))))))))

;helpers for getLocal
(define restOfParams cdr)
(define restOfParamValues cdr)
(define currentParam car)
(define currentParamValue car)

;helpers for getEnvironment
(define getParamsFromState
  (lambda (funName state)
    (car (lookup funName state))))

(define name cadr)
(define paramValues cddr)

;remove removes a variable from the state
; it takes the variable name and the state and removes it from the state
(define replace_var
  (lambda (var value state)
    (cond
      ((varsContain var (variables state)) (cons (get_replaced var value (currentLayer state)) (nextLayers state)))
      (else (cons (currentLayer state) (replace_var var value (nextLayers state)))))))

(define get_replaced
  (lambda (var value state)
    (cond
      ((eq? (variable1 state) var) (cons (cons var (restOfVars state)) (cons (cons value (restOfValues state)) '())))
      (else (currentLayer (insert (variable1 state) (valueOfVar1 state) (cons (get_replaced var value (cons (restOfVars state) (cons (restOfValues state) '()))) '())))))))

;insert inerts a variable into the state, if the value already exists it replaces it
;returns the state with a given variable and value added in
(define insert
  (lambda (var value state)
    (cond
      ((stateContains var state) (replace_var var value state))
      (else (cons (cons (cons var (variables state)) (cons (cons value (valuesInState state)) '())) (cdr state))))))

;createClosure creates a closure functon that will be added to the state
;the thirsd part of the cosure is the framework for the environment
(define createClosure
  (lambda (params body)
    (cons params (cons body '(((()())(()())))))))

;stateContains? checks if the variable has already been declared in the state
(define stateContains
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((varsContain var (variables state)) #t)
      (else (stateContains var (nextLayers state))))))

(define varsContain
  (lambda (var varList)
    (cond
     ((null? varList) #f)
     ((eq? var (var1 varList)) #t)
     (else (varsContain var (cdr varList)))))) 

;helper for state contains
(define var1 car)

(define resOfVariablesInState cdr)

;adds a level of scope to the given state
(define addLevelOfScope
  (lambda (state)
    (cons '(()()) state)))

;remove the outer most level of scope
(define getInnerScope cdr)

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
(define variables caar)

;values in the state
(define valuesInState cadar)

;outerLevelVariables gets the variables in the outer most scope
(define outerLevelVariables caar)

;outerLevelValues gets the values in the outer most scope
(define outerLevelValues cadar)

;secondLevelVariables gets the variables in the outer most scope
(define secondLevelVariables caadr)

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
(define allValues cadar)

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

(define catch-body caddr)

(define finally cadddr)

(define finallyExpressions cadr)

;helper methods for Mstate-func
(define funcName cadr)

(define getParams caddr)

(define getBody cadddr)
