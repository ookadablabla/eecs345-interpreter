; EECS 345 Class Project 1
; James Hochadel and Andrew Marmorstein
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
  	; Feed file into parser
  	; Evaluate the parse tree returned by parser
  	; Return the appropriate value
  ))

;lookup gets the value for a given variable
;takes a variable name and the state and returns the value of that variable
(define lookup
  (lambda (var s)
    (cond
      ((null? (car s)) 'error)
      ((eq? (caar s) var) (caadr s))
      (else (lookup var (cons (cdar s) (cons (cdadr s) '())))))))