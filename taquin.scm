#lang racket

(provide taquin-acc-state?)
(provide taquin-adj-states)
(provide taquin-heuristic)
(provide manhatan-distance)
(provide heuristique-aux)
(provide heuristique-line)
(provide sup-list-length)
(provide transition)
(provide switch_same_line)
(provide find_gap)
(provide find_gap_aux)
(provide switch_same_row)
(provide taquin-acc-state-aux)
(provide line-is-good)
(provide taquin-make-state)

;;;;;;;;;;;;;;;;;;;;;,,heuristic
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

(define (taquin-heuristic ls)
    (heuristique-aux ls 0 0 (sup-list-length ls 0))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;adjacent states 
(define taquin-adj-states
    (lambda (ls)
        (let ((alphabet '(u d l r)) (position (find_gap ls 1 1)))
            (if (or (null? ls) (equal? position '(0 0 0)))  
                ls
                (map (lambda(x) (cons x (transition ls x position))) alphabet)
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

;;;;;;;;;;;;;;;;;;;;taquin acc state
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
;;;;;;;;;;;;;;;;taquin make

(define (taquin-make-state ls)
    ls
)