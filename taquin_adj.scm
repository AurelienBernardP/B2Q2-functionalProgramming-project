


(define taquin_adj
    (lambda (ls)
        (let ((alphabet '(u d l r)) (position (find_gap ls 1 1)))
            (if (or (null? ls) (equal? position '(0 0)))  
                '()
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
          (cond ((and (null? ls) (= line size)) (append (0 0) (list size)))
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
 
 

