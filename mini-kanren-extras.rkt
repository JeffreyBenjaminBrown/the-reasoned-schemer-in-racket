#|
(load "mini-kanren-extras.rkt")

To load the canonical implementation:
(require Racket-miniKanren/miniKanren/mk)

To load the one 'raco pkg install' installs:
(require minikanren)

Found here:
https://www.monolune.com/using-racket-for-the-reasoned-schemer/
|#

; The rest of this is from or for The Reasoned Schemer
(define succeed (== #t #t))
(define fail (== #t #f))
(define else succeed)

; Define #s.
(current-readtable
 (make-readtable (current-readtable)
                 #\s
                 'dispatch-macro
                 (lambda (a b c d e f) succeed)))

; Define #u.
(current-readtable
 (make-readtable (current-readtable)
                 #\u
                 'dispatch-macro
                 (lambda (a b c d e f) fail)))
