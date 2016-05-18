; EECS 345 Class Project
; James Hochadel and Andrew Marmorstein
;
; This file contains functions for manipulating the state of the program.

; Look up a value by name in the state. If the value cannot be found, throw
; an error.
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
      ((null? state) (error 'out-of-scope (format "Symbol ~a is out of scope or does not exist" var)))
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

;state-contains? checks if the variable has already been declared in the state
(define state-contains?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((env-contains-symbol? var (variables state)) #t)
      (else (state-contains? var (nextLayers state))))))

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


;values in the state
(define valuesInState cadar)

;gets the first variable in the state
(define variable1 caar)

;gets the value associated with the first variable in the state
(define valueOfVar1 caadr)

;rest of the variables in the state
(define restOfVars cdar)

;rest of the values in the state
(define restOfValues cdadr)