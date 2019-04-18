(define solver-aux (nb-answers sits adj acc-state?)
    (let ((answer(get-answer sits))))
    (if (equal? #f answer) (solver-aux(nb-answers (make-move sits adj acc-state?) adj acc-state?))
        (cons (get-word sits nb-answer) (lambda () (solver-aux (+ 1 nb-answers) sits adj acc-state?)))
    )
)

(define rp-solver(s adj acc-state?)
    (lambda ()
        (solver-aux 1 '(()(s)()) adj acc-state?)
        )
)



////should be good///////////
(define make-move (sits adj acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state?(cadr(set-first sits))) (make-move (set-rest sits) adj acc-state?)
            (set-union (make-move (set-rest sits) adj acc-state?)(make-move-aux (set-first sits) (adj (set-first sits))))    
        )
    )
)

(define make-move-aux (old-state new-states)
    (if (null? new-states) (set )
        (if (equal? 'sink (car new-states)) (make-move-aux old-state (cdr new-states))
            (if (set-member? (caddr old-state)(cadr new-states)) (make-move-aux old-state (cdr new-states))
                (make-new-state old-state (car new-states))
            )        
        )
    )
)

(define make-new-state (old-state new-pair)
    (list (cons (car new-pair)(car old-state)) (cdr new-pair) (set-add (caddr old-state) (cadr old-state)))
)
//////////until here///////////////////