; The Little Schemer says we should do this
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

; The rest of this is from The Reasoned Schemer
(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) #s)
     ((== 'cup x) #s)
     (else #u))))

(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))
; The simpler definition below works in some cases but doesn't play well with other similar procedures in the same call to (run #f).
; (define caro
;   (lambda (alist aval)
;     (== (car alist) aval)))

(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))

(define conso
  (lambda (head tail result)
    (== (cons head tail) result)))

(define nullo
  (lambda (x)
    (== '() x)))

(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))

(define listo
  (lambda (l)
    (conde
     ((nullo l) #s)
     ((pairo l)
      (fresh (d)
             (cdro l d)
             (listo d)))
     (else #u))))

(define lolo ; list of lists
  (lambda (l)
    (conde
     ((nullo l) #s)
     ((fresh (a)
             (caro l a)
             (listo a))
      (fresh (d)
             (cdro l d)
             (lolo d)))
     (else #u))))

(define twinso
  (lambda (s)
    (fresh (x y)
           (conso x y s)
           (conso x '() y))))

; does this not work?
(define twinso2
  (lambda (l)
    (fresh (x)
           (== l (list x x)))))

(define loto
  (lambda (l)
    (conde
      ((nullo l) #s)
      (  (fresh (a)
                (caro l a)
                (twinso a))
         (fresh (d)
                (cdro l d)
                (loto d)))
      (else #u))))

(define listofo
  (lambda (predo l)
    (conde
      ((nullo l) #s)
      ( (fresh (a)
               (caro l a)
               (predo a))
        (fresh (d)
               (cdro l d)
               (listofo predo d)))
      (else #u))))

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde ((nullo l) #u)
           ((eq-caro l x) #s)
           (else (fresh (d)
                        (cdro l d)
                        (membero x d))))))

(define identity
  (lambda (l)
    (run* (y) (membero l y))))

(define pmembero_1
  (lambda (x l)
    (conde
     ((nullo l) #u)
     ((eq-caro l x) (cdro l '()))
     (else
      (fresh (d)
             (cdro l d)
             (pmembero_1 x d))))))

(define pmembero_2
  (lambda (x l)
    (conde
     ((nullo l) #u)
     ((eq-caro l x) (cdro l '()))
     ((eq-caro l x) #s)
     (else
      (fresh (d)
             (cdro l d)
             (pmembero_2 x d))))))

(define pmembero
  (lambda (x l)
    (conde
     ((eq-caro l x) (cdro l '()))
     ((eq-caro l x)
      (fresh (a d)
             (cdro l `(,a . ,d))))
     (else
      (fresh (d)
             (cdro l d)
             (pmembero_3 x d))))))

; TODO broken; does not reverse its input
(define memberrevo
  (lambda (x l)
    (conde
     (#s (fresh (d)
                (cdro l d)
                (memberrevo x d)))
     (else (eq-caro l x)))))

(define mem
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((eq-car? l x) l)
     (else (mem x (cdr l))))))

(define memo
  (lambda (x l out)
    (conde
     ((nullo l) #u)
     ((eq-caro l x) (== l out))
     (else
      (fresh (d)
             (cdro l d)
             (memo x d out))))))

(define rember
  (lambda (x l)
    (cond
     ((null? l) '())
     ((eq-car? l x) (cdr l))
     (else
      (cons (car l)
            (rember x (cdr l)))))))

(define rembero
  (lambda (x l out)
    (conde
     ((nullo l) (== '() out))
     ((eq-caro l x) (cdro l out))
     (else
      (frech (a d rec) ; rec ~ out, but recursed on d
             (conso a d l)
             (rembero x d rec)
             (conso a rec out))))))

(define surpriseo
  (lambda (s)
    (rembero s `(a b c) `(a b c))))

(define appendo
  (lambda (l s out)
    (conde ((nullo l) (== s out))
           (else 
            (fresh (a d rec) ; rec ~ out, but recursed on d
                   (conso a d l)
                   (conso a rec out)
                   (appendo d s rec))))))

(define unwrap
  (lambda (x)
    (cond
     ((pair? x) (unwrap (car x)))
     (else x))))

(define unwrapo
  (lambda (x out)
    (conde ( #s (== x out))
           (else (pairo x)
                 (fresh (a)
                        (caro x a)
                        (unwrapo a out)))
          )))

(define flatteno
  (lambda (s out)
    (conde ((nullo s) (== '() out))
           ((pairo s)
            (fresh (a af d df)
                   (conso a d s)
                   (flatteno a af)
                   (flatteno d df)
                   (appendo af df out)))
           (else (conso s '() out)))))

(define flattenrevo
  (lambda (s out)
    (conde (#s (conso s '() out))
           ((nullo s) (== '() out))
           (else (fresh (a af d df)
                        (conso a d s)
                        (flatteno a af)
                        (flatteno d df)
                        (appendo af df out)))
           )))

(define anyo
  (lambda (g)
    (conde
     (g #s)
     (else (anyo g)))))
(define nevero (anyo #u))
(define alwayso (anyo #s))
