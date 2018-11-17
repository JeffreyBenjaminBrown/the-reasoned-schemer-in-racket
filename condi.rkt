#| 
Unfortunately, I don't yet detect a difference between the behavior of the condi defined here and the conde that's already part of the library.

To write this, I

(1) copied the definition of conde, and everything it uses that needs modification, from Racket's canonical mini-kanren implementation:
https://github.com/miniKanren/Racket-miniKanren/blob/master/miniKanren/mk.rkt

(2) changed some names to indicate `i`, and 

(3) swapped the last two arguments to mplusi.
|#

(define-syntax condi
  (syntax-rules
   ()
   ((_ (g0 g ...) (g1 g^ ...) ...)
    (lambdag@ (a) 
              (inc 
               (mplusi* 
                (bindi* (g0 a) g ...)
                (bindi* (g1 a) g^ ...) ...))))))

(define mplusi
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplusi (f) f^)))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplusi (f^) f)))))))

(define-syntax mplusi*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (mplusi e0 
            (lambdaf@ () (mplusi* e ...))))))

(define-syntax bindi*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bindi* (bindi e g0) g ...))))

(define bindi
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bindi (f) g)))
      ((a) (g a))
      ((a f) (mplusi (g a) (lambdaf@ () (bindi (f) g)))))))
