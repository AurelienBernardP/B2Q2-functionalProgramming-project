(define (merge-sits new-sits old-sits)
    (if(null? new-sits) old-sits
        (merge-sits (cdr new-sits)(merge-aux(car new-sits) old-sits) )    
    )
)

(define (merge-aux new-sit old-sits)
    (if (null? old-sits) new-sit
        (if(< (car new-sit) (caar old-sits)) (cons new-sit old-sits)
            (cons (car old-sits) (merge-aux new-sit (cdr old-sits)))
        )    
    )     
)

(define (make-move-heu sits adj acc-state? heuristique)
    (if (null? sits) '() 
        (if (= 0 (caar sits)) (make-move-heu (cdr sits) adj acc-state? heuristique)
            (merge-sits (make-move-heu-aux (car sits) (adj (caddr sits) heuristique)) (cdr sits))
        )
    )    
)

(define (make-move-heu-aux old-sit new-states heuristique)
    (if (null? new-states) '()
        (if(equal? '(sink) (cdar new-states)) (make-move-heu-aux old-sit (cdr new-states) heuristique)
            (if(set-member? (cadddr old-sit) (cadar new-states)) (make-move-heu-aux old-sit (cdr new-states) heuristique)
                (cons (make-move-heu-aux old-sit (cdr new-states) heuristique) (make-new-state-heu old-sit (car new-states) heuristique))
            )
        )
    )
)

(define (make-new-state old-sit new-pair heuristique)
    (list (heuristique (cadr new-pair)) (cons (car new-pair)(cadr old-sit)) (set-add(cadddr old-sit)(cadr old-sit)))
)

(define (rp-solve-heuristique s adj acc-state? heuristique )
    (lambda ()
        (solver-heu-aux (list(list (heuristique s) '() s '())) '() adj acc-state? heuristique)
    )
)

(define (solver-heu-aux sits answers adj acc-state? heuristique)
    (if(null? sits) '()
        (let ((new-answers (find-answers-heu sits answers)))
            (if (null? new-answers) (solver-heu-aux (make-move-heu sits adj acc-state? heuristique) answers adj acc-state? heuristique) )
            (cons (reverse(car new-answers)) (lambda () (solver-heu-aux (remove-answers-heu sits) (cdr new-answers) adj acc-state? heuristique)))
        )
    )
)

(define (find-answers-heu sits answers)
    (if (null? sits) answers
        (if(<(caar sits) 0) answers
             (find-answers (cdr sits) (cons (cadar sits) answers))
        )
    )
)

(define (remove-answers-heu sits)
    (if (null? sits) '()
            (if(= (caar sits) 0)  (remove-answers-heu (cdr sits))
                sits
            )
    )
)