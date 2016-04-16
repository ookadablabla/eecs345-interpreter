; Test file for the interpreter project.
(load "interpreter.scm")

; Run all tests and format their results to be easily readable.
(define run-all-tests
  (lambda ()
    (let ((test-list (generate-integers 6 '())))
      (display (list->stringlist (map format-test-result (map run-test test-list) test-list))))))

; Run a single test.
(define run-test
  (lambda (test-number)
    (interpret (string-append (string-append "tests/3-" (number->string test-number) ".txt")))))

; Given a test result t and test number n, return "Test n: t".

; There is likely a better way to build strings than string-append, but I am
; insufficiently motivated to find it.
(define format-test-result
  (lambda (test-result test-number)
    (let* ((test-number-string (number->string test-number))
           (test-info (string-append (string-append "Test " test-number-string) ": ")))
      (string-append test-info (number->string test-result)))))

; Given a list of strings, return a single string containing each element of l, separated by
; the newline character \n.
(define list->stringlist
  (lambda (l)
    (if (null? l)
        ""
        (string-append (string-append (car l) "\n") (list->stringlist (cdr l))))))

; Generate a list of integers in ascending order ending with num
(define generate-integers
  (lambda (num l)
    (cond
      ((null? l) (generate-integers (- num 1) (cons num l)))
      ((eq? (car l) 1) l)
      (else (generate-integers (- num 1) (cons num l))))))
