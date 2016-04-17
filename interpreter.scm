; EECS 345 Class Project 2
; James Hochadel and Andrew Marmorstein
;
; This code was restructured using solution2.scm from Blackboard to better abstract certain
; functions and generally clean up Mstate.
(load "functionParser.scm")

; Interpret a file containing c code.
(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (let* ((initial-return (lambda (statement env) (return (Mvalue (operand statement) env return default-break default-continue default-throw))))
               (outer-environment (do-interpret (parser filename) initial-env (lambda (statement env) (return env)) default-break default-continue default-throw))
               (begin-interpret (lambda (env) (Mvalue-funcall (mainFuncall main) env initial-return default-break default-continue default-throw))))

              ; Begin interpreting. Pass in the environment, which is built by interpreting the outermost layer
              ; of the program, containing function and global variable definitions.
              (begin-interpret (getFunctionExecutionEnvironment (mainFuncall main) outer-environment initial-return default-break default-continue default-throw)))))))

(define main '((funcall main)))
(define mainFuncall car)

; do-interpret recursively evaluates statements and modifies the state appropriately
; based on their contents.
(define do-interpret
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (do-interpret (restOfExpressions statement)
                    (Mstate (firstExpression statement) state return break continue throw)
                    return break continue throw))))

(define initial-env '(((true false) (true false))))
(define default-break (lambda (s) (error 'invalidBreak "Break was called outside of a while loop")))
(define default-continue (lambda (s) (error 'invalidContinue "Continue was called outside of a while loop")))
(define default-throw (lambda (e s) (error 'uncaughtException "An exception was thrown but not caught")))

; Mstate modifies the state depending on the contents of statement.
(define Mstate
  (lambda (statement state return break continue throw)
    (cond
      ((eq? (operator statement) '=) (Mstate-assignment statement state return break continue throw))
      ((eq? (operator statement) 'begin) (Mstate-begin (cdr statement) state return break continue throw))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'if) (Mstate-if statement state return break continue throw))
      ((eq? (operator statement) 'return) (return statement state))
      ((eq? (operator statement) 'throw) (throw (exception statement) state))
      ((eq? (operator statement) 'try) (Mstate-tcf statement state return break continue throw))
      ((eq? (operator statement) 'var) (Mstate-var statement state return break continue throw))
      ((eq? (operator statement) 'function) (Mstate-func statement state))
      ((eq? (operator statement) 'funcall) (Mstate-funcall statement state return break continue throw))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state return new-break continue throw))))
      (else (error 'unknown "Encountered an unknown statement")))))


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

(define try-body cadr)
(define catch-body (lambda (v) (caddr (caddr v))))
(define catch-block caddr)
(define catch-err (lambda (v) (car (cadr (caddr v)))))
(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

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
      ((eq? 'true (Mbool (if-condition statement) state return break continue throw)) (Mstate (if-statement statement) state return break continue throw))
      ((not (null? (else-statement-exists statement))) (Mstate (else-statement statement) state return break continue throw))
      (else state))))

; Mstate-while handles while loops
(define Mstate-while
  (lambda (condition statement state return break continue throw)
    (if (eq? 'true (Mbool condition state return break continue throw))
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
  (lambda (statement state r b c t)
    (cond
      ;((stateContains (variable statement) state) (error 'redefining (format "Variable ~a has already been declared" (variable statement))))
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined state))
      (else (insert (variable statement) (Mvalue (operation statement) state r b c t) state)))))

;Mstate-func handles function declarations
(define Mstate-func
  (lambda (statement state)
    (cond
      ((stateContains (funcName statement) state) (error 'redefining (format "function ~a has already been declared" (funcName statement))))
      (else (insert (funcName statement) (createClosure (getParams statement) (getBody statement)) state)))))

;helper methods for Mstate-func
(define funcName cadr)
(define getParams caddr)

; Mstate-assignment handles variable assignment
(define Mstate-assignment
  (lambda (statement state r b c t)
    (replace_var (variable statement) (Mvalue (operation statement) state r b c t) state)))

; Mvalue: Evaluate an expression to determine its value.
; Last params are return break continue throw. Shortened for brevity.
(define Mvalue
  (lambda (statement state r b c t)
    (cond
      ((number? statement) statement)
      ((eq? statement 'true) 'true)
      ((eq? statement 'false) 'false)
      ((not (list? statement)) (lookup statement state))
      ((eq? (operator statement) '+) (+ (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)))
      ((eq? (operator statement) '-) (if (null? (cddr statement))
                                         (- (Mvalue (operand1 statement) state r b c t)) ; unary "-"
                                         (- (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t))))
      ((eq? (operator statement) '*) (* (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)))
      ((eq? (operator statement) '/) (quotient (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)))
      ((eq? (operator statement) '%) (remainder (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)))
      ((eq? (operator statement) 'funcall) (Mvalue-funcall statement state r b c t))
      (else (Mbool statement state r b c t)))))

;Mstate-funcall after the function is called
(define Mstate-funcall
  (lambda (funcall state return break continue throw)
    (call/cc
      (lambda (new-return)
        (let* ((func-name (funcName funcall))
               (function (lookup func-name state)))
              (do-interpret (getFuncBody function)
                            (getFunctionExecutionEnvironment funcall state return break continue throw)
                            (lambda (statement state) (new-return state))
                            break
                            continue
                            throw))))))

    ;(Mstate-funcall-with-originState funcall state state return break continue throw)))

;(define Mstate-funcall-with-originState
;  (lambda (funcall state originState return break continue throw)
;    (if (env-contains-symbol? (funcName funcall) (variables state))
;      (globalStateOfEnvironment (do-interpret (getFuncBody (lookup (funcName funcall) state))
;                                              ((getFuncEnvironment (lookup (funcName funcall) state)) funcall originState)
;                                              return break continue throw))
;      (else (cons (currentLayer state) (Mstate-funcall-with-originState funcall (nextLayers state) originState return break continue throw))))))

;helpers for Mstate-funcall
(define globalStateOfEnvironment cdr)
(define getFuncBody cadr)
(define getFuncEnvironment caddr)
(define getBody cadddr)

; When a function is called, Mvalue-funcall does the following:
; 1. Creates the function's execution environment using the environment function stored
;    in the function closure
; 2. Binds the actual parameters to the formal parameters in the new environment
; 3. Evaluates the body of the function.
;
; Differing
; Execute a function and return the value produced by its return statement.
; TODO: Match env-contains-symbol? check from Mstate-funcall
(define Mvalue-funcall
  (lambda (statement state return break continue throw)
    (call/cc
      (lambda (new-return)
        (let* ((func-name (funcName statement))
               (function (lookup func-name state)))
              (do-interpret (getFuncBody function)
                            (getFunctionExecutionEnvironment statement state return break continue throw)
                            (lambda (statement state) (new-return (Mvalue (operand statement) state return break continue throw)))
                            break
                            continue
                            throw))))))

; getEnvironment gets the environment within which a function call has access
; Assumes funcall is of format (funcall methodName actual-param-1 actual-param-2 ...)
(define getFunctionExecutionEnvironment
  (lambda (funcall state r b c t)
    (getEnvironment (name funcall) (getParamsFromState (name funcall) state) (paramValues funcall) state r b c t)))

(define getEnvironment
  (lambda (funName funParams funParamValues state r b c t)
    (cons (getLocal funParams funParamValues state r b c t) (getGlobal funName state))))

;getGlobal gets the global variables for the environment
(define getGlobal
  (lambda (funName state)
    (cond
      ((env-contains-symbol? funName (variables state)) state)
      (else (getGlobal funName (nextLayers state))))))

;getLocal get all of the local variable for the function which will be the parameters
(define getLocal
  (lambda (funParams paramValues state r b c t)
    (getLocalWithFormat funParams paramValues state '(()()) r b c t)))

(define getLocalWithFormat
  (lambda (funParams paramValues state localState r b c t)
    (cond
      ((and (null? funParams) (not (null? paramValues))) (error 'invalid (format "too many parameters")))
      ((null? funParams) localState)
      (else (getLocalWithFormat (restOfParams funParams)
                                (restOfParamValues paramValues)
                                state
                                (currentLayer (insert (currentParam funParams) (Mvalue (currentParamValue paramValues) state r b c t) (cons localState '())))
                                r b c t)))))

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

; Mbool: Evaluate a statement for a truth value of true or false.
(define Mbool
  (lambda (statement state r b c t)
    (cond
      ((not (list? statement)) (Mvalue statement state r b c t))
      ((eq? statement 'true) 'true)
      ((eq? statement 'false) 'false)
      ((not (list? statement)) (Mvalue statement state r b c t))
      ((eq? (comparator statement) '>) (if (> (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '<) (if (< (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '>=) (if (>= (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '<=) (if (<= (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '==) (if (= (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '!=) (if (not (= (Mvalue (operand1 statement) state r b c t) (Mvalue (operand2 statement) state r b c t))) 'true 'false))
      ((eq? (comparator statement) 'funcall) (Mvalue statement state r b c t))
      ((eq? (operator statement) '&&) (if (eq? #t (and (eq? 'true (Mbool (operand1 statement) state r b c t)) (eq? 'true (Mbool (operand2 statement) state r b c t)))) 'true 'false))
      ((eq? (operator statement) '||) (if (eq? #t (or (eq? 'true (Mbool (operand1 statement) state r b c t)) (eq? 'true (Mbool (operand2 statement) state r b c t)))) 'true 'false))
      ((eq? (operator statement) '!) (if (eq? #t (not (eq? 'true (Mbool (operand1 statement) state r b c t)))) 'true 'false))
      (else (error 'invalidInput "This expression cannot be evaluated to a boolean value")))))

; HELPER METHODS

(define lookup
  (lambda (var state)
    (cond
      ((null? state) (error 'unknown (format "Symbol ~a does not exist" var)))
      ((env-contains-symbol? var (variables state)) (lookupVal var (currentLayer state)))
      (else (lookup var (nextLayers state))))))

(define lookupVal
  (lambda (var state)
    (cond
      ((eq? (variable1 state) var) (unbox (valueOfVar1 state)))
      (else (lookupVal var (cons (restOfVars state) (cons (restOfValues state) '())))))))


;helpers for lookup
(define nextLayers cdr)

(define currentLayer car)

(define variableList caar)

; remove removes a variable from the state
; it takes the variable name and the state and removes it from the state
(define replace_var
  (lambda (var value state)
    (cond
      ((null? state) (error 'out-of-scope (format "variable ~a is out of scope" var)))
      ((env-contains-symbol? var (variables state)) (cons (get_replaced var value (currentLayer state)) (nextLayers state)))
      (else (cons (currentLayer state) (replace_var var value (nextLayers state)))))))

(define get_replaced
  (lambda (var value state)
    (cond
      ((eq? (variable1 state) var) (cons (cons var (restOfVars state)) (cons (cons (begin (set-box! (valueOfVar1 state) value) (valueOfVar1 state)) (restOfValues state)) '())))
      (else (currentLayer (insert (variable1 state) (unbox (valueOfVar1 state)) (cons (get_replaced var value (cons (restOfVars state) (cons (restOfValues state) '()))) '())))))))

;insert inerts a variable into the state, if the value already exists it replaces it
;returns the state with a given variable and value added in
(define insert
  (lambda (var value state)
    (cons (cons (cons var (variables state)) (cons (cons (box value) (valuesInState state)) '())) (cdr state))))

;createClosure creates a closure functon that will be added to the state
;the thirsd part of the cosure is the framework for the environment
(define createClosure
  (lambda (params body)
    (cons params (cons body (cons getFunctionExecutionEnvironment '())))))

;stateContains? checks if the variable has already been declared in the state
(define stateContains
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((env-contains-symbol? var (variables state)) #t)
      (else (stateContains var (nextLayers state))))))

(define env-contains-symbol?
  (lambda (var varList)
    (cond
     ((null? varList) #f)
     ((eq? var (var1 varList)) #t)
     (else (env-contains-symbol? var (cdr varList))))))

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

;operand1
(define operand1 cadr)

; operand
(define operand operand1)

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

(define exception cadr)
