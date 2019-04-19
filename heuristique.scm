(define (manhatan-distance elem col line size)
    (if (eq? elem 'x) 0 
        (+(abs(-(modulo (- elem 1) size)col))
        (abs(-(quotient (- elem 1) size)line))
        )
    )    
)

(define (heuristique-aux ls col line size)
    (if (null? ls) 0
        (+ (heuristique-line (car ls) col line size) (heuristique-aux (cdr ls) col (+ 1 line) size))
    )
)

(define (heuristique-line ls col line size)
    (if (null? ls) 0
        (+ (manhatan-distance (car ls) col line size) (heuristique-line (cdr ls) (+ 1 col) line size) )
    )
)

(define sup-list-length 
    (lambda (ls counter)
        (if (null? ls)counter 
            (sup-list-length (cdr ls)(+ counter 1)))))

(define (taquin-heuristique ls)
    (heuristique-aux ls 0 0 (sup-list-length ls 0))
)