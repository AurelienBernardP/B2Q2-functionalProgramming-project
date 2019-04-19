#lang racket
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
            (merge-sits (make-move-heu-aux (car sits) (adj (caddar sits))heuristique) (cdr sits))
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

(define (make-new-state-heu old-sit new-pair heuristique)
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
            (if (null? new-answers) (solver-heu-aux (make-move-heu sits adj acc-state? heuristique) new-answers adj acc-state? heuristique)
                (cons (reverse(car new-answers)) (lambda () (solver-heu-aux (remove-answers-heu sits) (cdr new-answers) adj acc-state? heuristique)))
            )
        )
    )
)

(define (find-answers-heu sits answers)
    (if (null? sits) answers
        (if(>(caar sits) 0) answers
             (find-answers-heu (cdr sits) (cons (cadar sits) answers))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; below this line everything is working
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

(define taquin_adj
    (lambda (ls)
        (let ((alphabet '(u d l r)) (position (find_gap ls 1 1)))
            (if (or (null? ls) (equal? position '(0 0 0)))  
                ls
                (map (lambda(x) (cons x (list (transition ls x position)))) alphabet)
            )
        )
    )
)

(define transition
    (lambda (ls symbol position)
        (cond   ((equal? symbol 'u) (if (= (car position) 1)
                                        'sink
                                        (switch_same_row ls '() (- (car position) 1) (cadr position))))
                ((equal? symbol 'd) (if (= (car position) (caddr position))
                                        'sink
                                        (switch_same_row ls '() (car position) (cadr position))))
                ((equal? symbol 'l) (if(= (cadr position) 1)
                                        'sink
                                        (switch_same_line ls (car position) (- (cadr position) 1))))
                ((equal? symbol 'r) (if(= (cadr position) (caddr position))
                                        'sink                        
                                        (switch_same_line ls (car position) (cadr position))))
                (else ls)
        )
    )
)

(define switch_same_line
    (lambda (ls line row)
       (cond ((null? ls) '())
             ((> line 1) (cons (car ls) (switch_same_line (cdr ls) (- line 1) row)))
             ((list? (car ls)) (cons (switch_same_line (car ls) line row) (cdr ls)))
             ((= row 1) (append (list (cadr ls) (car ls)) (cddr ls)))
             (else (cons (car ls) (switch_same_line (cdr ls) line (- row 1))))
       )
    )
 )

 ;attention si N sous listes alros sous listes de taille N
 (define find_gap
    (lambda (ls line size)
       (let ((x (find_gap_aux ls 1)))
          (cond ((and (null? ls) (= line size)) '(0 0 0))
                ((and (null? ls) (not (= line size))) (list (- size 1)))
                ((> x 0) (append (list line x) (find_gap (cdr ls) line (+ size 1))))
                (else (find_gap (cdr ls) (+ line 1) (+ size 1)))
          )
       )
    )
 )
 
 (define find_gap_aux 
    (lambda (ls row)
       (cond ((null? ls) 0)
             ((list? (car ls)) (find_gap_aux (car ls) row))
             ((equal? (car ls) 'x) row)
             (else (find_gap_aux (cdr ls) (+ row 1)))
       )
    )
 )
 
 (define switch_same_row
    (lambda (ls rs line row)
       (cond ((null? ls) '())
             ((> line 1) (cons (car ls) (switch_same_row (cdr ls) rs (- line 1) row)))
             ((list? (car ls)) (append (switch_same_row (car ls) (cadr ls) line row) (cddr ls)))
             ((= row 1) (list (cons (car rs) (cdr ls)) (cons (car ls) (cdr rs))))
             (else 
                (let ((x (switch_same_row (cdr ls) (cdr rs) line (- row 1))))
                   (list (cons (car ls) (car x)) (cons (car rs) (cadr x)))
                )
             )
       )
    )
 )
(define taquin-acc-state?
    (lambda (ls) 
        (taquin-acc-state-aux ls 1 1 (sup-list-length ls 0))))

(define taquin-acc-state-aux
    (lambda (ls col line size)
        (if (null? ls) #t 
            (and (line-is-good (car ls) 1 line size ) (taquin-acc-state-aux (cdr ls) col (+ line 1) size )))))

(define line-is-good 
    (lambda (ls col line size)
        (if (null? ls) #t 
            (if (or (eq? (car ls) (+ (* size (- line 1)) col)) (and ( eq? (car ls) 'x) (= col line size))) (line-is-good (cdr ls) (+ col 1) line size )
                    #f))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;testing fcts;;;;;;;;;;;;;;;
(define sits-test-good (list(list (taquin-heuristique '((1 2 3)(4 5 6)(7 8 x))) '() '((1 2 3)(4 5 6)(7 8 x)) '())))
(define sits-test-bad (list(list (taquin-heuristique '((1 2 3)(4 5 6)(x 8 7))) '() '((1 2 3)(4 5 6)(x 8 7)) '())))
(define test-it (rp-solve-heuristique '((1 2 3)(4 x 6)(7 8 5)) taquin_adj taquin-acc-state? taquin-heuristique ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n-solutions
  (lambda (iterator n)
    (if (zero? n) '()
        (let ((pair (iterator)))
          (if (null? pair) '()
              (cons (car pair) (n-solutions (cdr pair) (- n 1))))))))
