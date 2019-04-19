#lang racket


(define (find-answers sits answers acc-state?)
    (if (set-empty? sits) answers
        (if (acc-state? (cadr(set-first sits))) (find-answers (set-rest sits) (cons (car(set-first sits)) answers) acc-state?)
            (find-answers (set-rest sits) answers acc-state?)
        )
    )
)

(define (solver-aux sits adj acc-state? answers)
    (if (set-empty? sits) '()
        (let ((new-answers (find-answers sits answers acc-state? )))
          (if (null? new-answers) (solver-aux (make-move sits adj acc-state?) adj acc-state? new-answers)
              (cons (reverse(car new-answers)) (lambda () (solver-aux (remove-answers sits acc-state?) adj acc-state? (cdr new-answers))))
          )
        )
    )
)
(define (remove-answers sits acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state? (cadr(set-first sits))) (remove-answers (set-rest sits) acc-state?)
            (set-add (remove-answers (set-rest sits) acc-state?) (set-first sits))
        )
    )
)

(define (rp-solver s adj acc-state?)
    (lambda ()
        (solver-aux (set(list '() s '())) adj acc-state? '())
    )
)


(define (make-move sits adj acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state? (cadr(set-first sits))) (make-move (set-rest sits) adj acc-state?)
            (set-union (make-move-aux (set-first sits) (adj(cadr(set-first sits))))(make-move (set-rest sits) adj acc-state?))    
        )
    )
)

(define (make-move-aux old-state new-states)
    (if (null? new-states) (set )
        (if (equal? '(sink) (cdar new-states)) (make-move-aux old-state (cdr new-states))
            (if (set-member? (caddr old-state)(cadar new-states)) (make-move-aux old-state (cdr new-states))
                (set-add (make-move-aux old-state (cdr new-states)) (make-new-state old-state (car new-states)))
            )        
        )
    )
)

(define (make-new-state old-state new-pair)
    (list (cons (car new-pair)(car old-state)) (cadr new-pair) (set-add (caddr old-state) (cadr old-state)))
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

(define sup-list-length 
    (lambda (ls counter)
        (if (null? ls)counter 
            (sup-list-length (cdr ls)(+ counter 1)))))

(define taquin-acc-state-aux
    (lambda (ls col line size)
        (if (null? ls) #t 
            (and (line-is-good (car ls) 1 line size ) (taquin-acc-state-aux (cdr ls) col (+ line 1) size )))))

(define line-is-good 
    (lambda (ls col line size)
        (if (null? ls) #t 
            (if (or (eq? (car ls) (+ (* size (- line 1)) col)) (and ( eq? (car ls) 'x) (= col line size))) (line-is-good (cdr ls) (+ col 1) line size )
                    #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test-set-bad (set(list '() '((2 3 6) (1 x 5) (7 8 4)) '())))
(define test-set-good (set(list '() '((1 2 3) (4 5 6) (7 8 x)) '())))
(define test-adj (taquin_adj '((1 2)(x 3))))
(define adj-set (taquin_adj(cadr(set-first test-set-bad))))
(define test-it (rp-solver '((1 x 3) (5 6 7) (8 4 2)) taquin_adj taquin-acc-state?))
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n-solutions
  (lambda (iterator n)
    (if (zero? n) '()
        (let ((pair (iterator)))
          (if (null? pair) '()
              (cons (car pair) (n-solutions (cdr pair) (- n 1))))))))
