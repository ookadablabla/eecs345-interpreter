; EECS 345 Class Project
; James Hochadel and Andrew Marmorstein
;
; This code was restructured using solution2.scm from Blackboard to better abstract certain
; functions and generally clean up Mstate.
(load "functionParser.scm")

; Interpret a file containing Java-like code.
;
; Setup:
; 1. Create initial-return, which accepts a statement and the environment from which the return
;    was called, evaluates the statement, and returns the result.
; 2. Create outer-environment, which contains all globally accessible functions and variables.
; 3. Create begin-interpret, a function that calls the file's main method with default return,
;    break, continue, and throw.
;
; Execution: Run begin-interpret. Pass it outer-environment with an empty layer for main's local
; variable and function definitions.
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

; Mstate modifies the state depending on the contents of statement, then returns the state..
; TODO: Move while's continuation to Mstate-while
(define Mstate
  (lambda (statement state return break continue throw)
    (cond
      ((eq? (operator statement) '=) (Mstate-assignment statement state return break continue throw))
      ((eq? (operator statement) 'begin) (Mstate-begin (cdr statement) state return break continue throw))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'funcall) (Mstate-funcall statement state return break continue throw))
      ((eq? (operator statement) 'function) (Mstate-func statement state))
      ((eq? (operator statement) 'if) (Mstate-if statement state return break continue throw))
      ((eq? (operator statement) 'return) (return statement state))
      ((eq? (operator statement) 'throw) (throw (exception statement) state))
      ((eq? (operator statement) 'try) (Mstate-tcf statement state return break continue throw))
      ((eq? (operator statement) 'var) (Mstate-var statement state return break continue throw))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state return new-break continue throw))))
      (else (error 'unknown "Encountered an unknown statement")))))

(define exception cadr)

; Mstate-assignment handles variable assignment
(define Mstate-assignment
  (lambda (statement state r b c t)
    (replace_var (variable statement) (Mvalue (operation statement) state r b c t) state)))

; Whenever entering a block of code with curly braces, this function should be called to evaluate
; the contents of the block inside a new layer of scope.
; Statement format:
; (begin (stmt-1) (stmt-2) ...)
(define Mstate-begin
  (lambda (statement state return break continue throw)
    (getInnerScope (do-interpret statement
                                 (addLevelOfScope state)
                                 return
                                 (lambda (s) (break (getInnerScope s)))
                                 (lambda (s) (continue (getInnerScope s)))
                                 (lambda (e s) (throw e (getInnerScope s)))))))

; Mstate-if handles if statements
; Statement format: (else-statement is optional)
; (if (condition) (statement) (else-statement))
(define Mstate-if
  (lambda (statement state return break continue throw)
    (cond
      ((eq? 'true (Mbool (if-condition statement) state return break continue throw)) (Mstate (if-statement statement) state return break continue throw))
      ((not (null? (else-statement-exists statement))) (Mstate (else-statement statement) state return break continue throw))
      (else state))))

(define else-statement-exists cdddr)
(define if-condition cadr)
(define if-statement caddr)
(define else-statement cadddr)

; Mstate-func handles function declarations
; Statement format:
; (function function-name (formal-param-1, formal-param-2, ...) (body))
(define Mstate-func
  (lambda (statement state)
    (cond
      ((stateContains (funcName statement) state) (error 'redefining (format "function ~a has already been declared" (funcName statement))))
      (else (insert (funcName statement) (createClosure (getParams statement) (getBody statement)) state)))))

;helper methods for Mstate-func
(define funcName cadr)
(define getParams caddr)

; Mstate-funcall after the function is called
; Statement format:
; (funcall function-name actual-param-1 actual-param-2 ...)
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

;helpers for Mstate-funcall
(define globalStateOfEnvironment cdr)
(define getFuncBody cadr)
(define getFuncEnvironment caddr)
(define getBody cadddr)

; Modify the state based on a try-catch-finally block.
; Statement format, where each "body" can consist of multiple statements in a list:
; (try (try-body) (catch (exception-name) (catch-body)) (finally (finally-body)))
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

; MState-var handles variable declaration
; Statement format:
; (var var-name) OR (var var-name value)
(define Mstate-var
  (lambda (statement state r b c t)
    (cond
      ;((stateContains (variable statement) state) (error 'redefining (format "Variable ~a has already been declared" (variable statement))))
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined state))
      (else (insert (variable statement) (Mvalue (operation statement) state r b c t) state)))))

; Mstate-while handles while loops
; TODO: check that continue actually works
; Statement format:
; (while (condition) (body))
; body may be one line only; for multiple lines, it must contain a begin.
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

(define parse-while-condition cadr)
(define parse-while-statement caddr)

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

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand operand1) ; TODO: Can this be moved / replaced?

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
  (lambda (statement env return break continue throw)
    (call/cc
      (lambda (new-return)
        (let* ((func-name (funcName statement))
               (function (lookup func-name env)))
              (do-interpret (getFuncBody function)
                            ; replace the below with a call to the function closure's create-env function
                            ; function in the closure should already pass the function name into getFunctionExecutionEnvironment
                            ; so that we don't have to do it here
                            (getFunctionExecutionEnvironment statement env return break continue throw)
                            (lambda (statement env) (new-return (Mvalue (operand statement) env return break continue throw)))
                            break
                            continue
                            throw))))))

; getFunctionExecutionEnviroinment gets the execution environment for a function call,
; which includes everything available to the function through static scoping along with
; its parameters.
; Assumes funcall is of format (funcall methodName actual-param-1 actual-param-2 ...)
; Return looks like
; (((formal-param-names)(actual-param-values)) ((declaration-scope-symbols)(declaration-scope-values)) ... ((global-symbols)(global-values)))
(define getFunctionExecutionEnvironment
  (lambda (funcall env r b c t)
    (cons (bindParameters (function-name funcall) (param-list funcall) env r b c t) (getFunctionDeclarationEnvironment (function-name funcall) env))))

(define function-name cadr)
(define param-list cddr)

; Returns an environment with all bindings within the function's scope - i.e.,
; all bindings available in the layer it was declared and above. Does not prepend
; an empty local scope. Based on static scoping.
; Return looks like:
; (((declaration-scope-symbols)(declaration-scope-values)) ... ((global-symbols)(global-values)))
(define getFunctionDeclarationEnvironment
  (lambda (funName env)
    (cond
      ((env-contains-symbol? funName (variables env)) env)
      (else (getFunctionDeclarationEnvironment funName (nextLayers env))))))

; Given the name of a function, the actual parameters being passed to the function,
; and the environment from which the function was called, locate the function closure
; in env and bind the actual parameters to the function's formal parameters.
; Return looks like:
; ((formal-param-names)(actual-param-values))
(define bindParameters
  (lambda (funcName actualParams env r b c t)
    (bindActualToFormal (getParamsFromEnvironment funcName env) actualParams env '(()()) r b c t)))

; Returns the list of formal parameters as stored in the function closure in the environment.
(define getParamsFromEnvironment
  (lambda (funName env)
    (car (lookup funName env))))

; Recursively bind the actual parameters to the formal parameters.
; Accepts the environment from which the function is being called and localEnv, which should
; be '(()()) on the first call.
; Not a great name but... meh
(define bindActualToFormal
  (lambda (formalParams actualParams env localEnv r b c t)
    (cond
      ; If we've reached the end of the formal or actual param list but not the other, the
      ; function was not called with the correct number of parameters and we throw an error.
      ((and (null? formalParams) (not (null? actualParams))) (error 'methodSignature (format "too many parameters")))
      ((and (not (null? formalParams)) (null? actualParams)) (error 'methodSignature (format "too few parameters")))
      ((null? formalParams) localEnv)
      (else (bindActualToFormal (restOfParams formalParams)
                                (restOfParamValues actualParams)
                                env
                                (currentLayer (insert (currentParam formalParams) (Mvalue (currentParamValue actualParams) env r b c t) (cons localEnv '())))
                                r b c t)))))

;helpers for bindActualToFormal
(define restOfParams cdr)
(define restOfParamValues cdr)
(define currentParam car)
(define currentParamValue car)

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

(define comparator car)

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

;variable
(define variable cadr)

;third element
(define thirdElement cddr)

;operation
(define operation caddr)
