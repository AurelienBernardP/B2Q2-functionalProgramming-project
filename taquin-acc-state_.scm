(define taquin-acc-state?
    (lambda (ls) 
        (taquin-acc-state-aux ls 0 0 (sup-list-length ls 0))))

(define sup-list-length 
    (lambda (ls counter)
        (if (null? ls)counter 
            (sup-list-length (cdr ls)(+ acc 1)))))

(define taquin-acc-state-aux
    (lambda (ls col line size)
        (if (null? ls) #t 
            (and (line-is-good (car ls) 0 size ) (taquin-acc-state-aux (cdr ls) (+ line 1) size )))))

(define line-is-good 
    (lambda (ls col line size)
        (if (null? ls) #t 
            (if (and ( equal? (car ls) 'x') (= col line size)) (line-is-good (cdr ls) (+ col 1) line size )
                (if (= (car ls) (+ (* size (- line 1)) col)) (line-is-good (cdr ls)(+ col 1) line size)
                    #f)))))