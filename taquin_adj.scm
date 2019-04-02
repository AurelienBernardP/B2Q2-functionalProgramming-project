(define taquin_adj
    (lambda (ls)
        (if (null?) '()
                    (taquin_adj_aux ls '(u d l r))
        )
    )
)

(define taquin_adj_aux
    (lambda (ls alphabet)
        (if (null? alphabet) '()
                             (cons (transition x (car alphabet)) (list (cdr alphabet)))
        )
    )
)

(define transition
    (lambda (ls symbol)
        (cond(   ((= symbol 'u) (transition-u ls (find_hole ls)))
                 ((= symbol 'd) (transition-d ls (find_hole ls)))
                 ((= symbol 'l) (transition-l ls (find_hole ls)))
                 ((= symbol 'r) (transition-r ls (find_hole ls)))
                 (else ls)
             )
        )
    )
)

(define find_hole
    (lambda (ls line)
        (cond   ((null? ls) '(0 0))
                ((list? (car ls)) (find_hole (car ls) (+ line 1)))
                (else (find_hole (car ls) line))
        )
    )
)

(define find_hole_aux
    (lambda (ls row)
        (cond   ((null? ls) 0)
                ((= (car ls) 'x) row)
                (else (find_hole_aux (cdr ls) (+ row 1)))
        )
    )
)



;       (cond((pair? (car ls)) find_hole(car(ls) (+ line 1) 0)) ;le row arg ici dépend de l'init   
;             ((= (car ls) 'x) (list line row))
;             ((null? (cdr ls)) (list 0 0))
;             (else (find_hole (cdr ls) line (+ row 1)))
;        )
;    )
;)

(define transition-l
    (lambda (ls position)
        (if (= (car position) 1) 
            (cons 'l 'sink)
            (cons 'l (exchange ls position))
        )
    )
)



