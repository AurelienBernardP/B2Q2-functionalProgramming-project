(define make-move (sits adj acc-state?)
    (if (null? sits) '()
        (if () (make-move (cdr sits) adj acc-state?)
            (let ((list1(new-sits(car sits) adj ) (list2(make-move(cdr sits acc-state?))))))
            (merge-sits list1 list2)
        )
    )
)


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

(define get-word (sits nb)
    (if (null? sits) 'unavailable-answer
        (if (= nb 1) (caar sits)
            (get-word(cdr sits) (- nb 1)) 
        ) 
    )
)

%%%%%%%%%%%%%%%%%%%%%%% counts the number of acceptance states in sits( by either using )
(define count-answers (sits acc-satate? nb-answers)


)

%%%%%%%%%%%%%%%%%%%%%compares two states
(define compare-state (sit1 sit2)



)


(define make-move (sits adj acc-state? solutions )
    (if (set-empty? sits) (set)
        (set-union (make-move (set-rest sits) adj acc-state solutions)(list->set (map (lambda(x)(make-move-aux (car(set-first sits)) x (caddr(set-first sits))acc-state solutions)))))    
    )
)

(define make-move-aux (current-word new-state forbiden-states acc-state solutions)
    (if (equal? new-state 'sink) '()
        (if )
    )

)