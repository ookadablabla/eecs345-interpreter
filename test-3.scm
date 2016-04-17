; Contains functions that allow automated testing of Part 3 of the interpreter project.
(load "interpreter.scm")

; Run all tests and format their results to be easily readable.
;
; TODO: Currently only runs up to test 11, because test 12 returns an error,
; and the code can't yet catch errors. I think the best option right now is
; somewhere on this page: https://docs.racket-lang.org/reference/exns.html, since
; we'll want to run this in DrRacket, which doesn't support mit-scheme (which does have
; good error handling).
(define run-all-tests
  (lambda ()
    (let ((test-list (generate-consecutive-integers 11 '())))
      (display (list->stringlist (map format-test-result (map run-test test-list) test-list))))))

; Run a single test.
(define run-test
  (lambda (test-number)
    (interpret (format "tests/3-~a.txt" (number->string test-number)))))

; Given a test result t and test number n, return "Test n: t".

; There is likely a better way to build strings than string-append, but I am
; insufficiently motivated to find it.
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