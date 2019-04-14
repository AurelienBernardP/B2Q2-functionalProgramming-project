(define make-move (sits adj acc-state?)
    (if (null? sits) '()
        (if (TODO someting here to skip already good states AND send an error IF all answers have been encounteredd) (make-move (cdr sits) adj acc-state?)
            (let ((list1(new-sits(car sits) adj ) (list2(make-move(cdr sits acc-state?))))))
            (merge-sits list1 list2)
        )
    )
)

(define new-sits)


(define solver-aux (nb-answers sits adj acc-state?)
    (if (count-aswers sits acc-state? nb-answers) (cons (get-word sits nb-answer) (lambda () (solver-aux (+ 1 nb-answers) sits adj acc-state?)))
        (solver-aux(nb-answers (make-move sits adj acc-state?) adj acc-state?))
    )
)

(define rp-solver(s adj acc-state?)
    (lambda ()
        (solver-aux 1 (make-state s) adj acc-state?)
        )
)