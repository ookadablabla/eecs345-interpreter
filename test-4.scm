; Contains functions that allow automated testing of Part 3 of the interpreter project.
(load "interpreter.scm")

; Run all tests and format their results to be easily readable.
(define run-all-tests
  (lambda ()
    (let ((test-list (generate-consecutive-integers 13 '())))
      (display (list->stringlist (map format-test-result (map run-test test-list) test-list))))))

; Run a single test.
(define run-test
  (lambda (test-number)
    (interpret (format "tests/4-~a.txt" (number->string test-number)))))

; Given a test result t and test number n, return "Test n: t".
(define format-test-result
  (lambda (test-result test-number)
      (format "Test ~a: ~a" (number->string test-number) test-result)))

; Given a list of strings, return a single string containing each element of l, separated by
; the newline character \n.
(define list->stringlist
  (lambda (l)
    (if (null? l)
        ""
        (string-append (format "~a\n" (car l)) (list->stringlist (cdr l))))))

; Generate a list of integers in ascending order ending with num.
;
; I feel like I'm missing some obvious simplification here, but who knows
(define generate-consecutive-integers
  (lambda (num l)
    (cond
      ((null? l) (generate-consecutive-integers (- num 1) (cons num l)))
      ((eq? (car l) 1) l)
      (else (generate-consecutive-integers (- num 1) (cons num l))))))