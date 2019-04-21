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
; Si `state` est la représentation d'un état, (taquin-adj-states state)
; renvoie une liste de paire pointées, dont le car est le symbole menant
; à l'état représenté par le cdr.
(define taquin-adj-states
    (lambda (state)
        (let ((alphabet '(u d l r)) (position (find_gap state 1 1)))
            (if (or (null? state) (equal? position '(0 0 0)))  
                state
                (map (lambda(x) (cons x (transition state x position))) alphabet)
            )
        )
    )
)

; Si `state` est la représentation d'un état, si symbol est une lettre de l'alphabet
; et position est une liste contenant respectivement les coordonnées du trou dans le
; le taquin et la taille du taquin, alors (transition state symbol position)
; renvoie l'état correspondant à l'état initial qui a subi l'action symbol.
(define transition
    (lambda (state symbol position)
        (cond   ((equal? symbol 'u) (if (= (car position) 1)
                                        'sink
                                        (switch_same_row state '() (- (car position) 1) (cadr position))))
                ((equal? symbol 'd) (if (= (car position) (caddr position))
                                        'sink
                                        (switch_same_row state '() (car position) (cadr position))))
                ((equal? symbol 'l) (if(= (cadr position) 1)
                                        'sink
                                        (switch_same_line state (car position) (- (cadr position) 1))))
                ((equal? symbol 'r) (if(= (cadr position) (caddr position))
                                        'sink                        
                                        (switch_same_line state (car position) (cadr position))))
                (else state)
        )
    )
)

; Si `state` est la représentation d'un état et si line et row correspondent à des
; coordonnées, alors (swtich_same_line state line row) renvoie l'état du taquin
; lorsque l'élément se trouvant à la ligne line et la colonne row est échangé
; avec son voisin de droite.
(define switch_same_line
    (lambda (state line row)
       (cond ((null? state) '())
             ((> line 1) (cons (car state) (switch_same_line (cdr state) (- line 1) row)))
             ((list? (car state)) (cons (switch_same_line (car state) line row) (cdr state)))
             ((= row 1) (append (list (cadr state) (car state)) (cddr state)))
             (else (cons (car state) (switch_same_line (cdr state) line (- row 1))))
       )
    )
 )

; Si `state` est la représentation d'un état, si rs est une liste et si line et row
; correspondent à des coordonnées, alors (swtich_same_row state rs line row) renvoie
; l'état du taquin lorsque l'élément se trouvant à la ligne line et la colonne row est
; échangé avec son voisin de haut.
 (define switch_same_row
    (lambda (state rs line row)
       (cond ((null? state) '())
             ((> line 1) (cons (car state) (switch_same_row (cdr state) rs (- line 1) row)))
             ((list? (car state)) (append (switch_same_row (car state) (cadr state) line row) (cddr state)))
             ((= row 1) (list (cons (car rs) (cdr state)) (cons (car state) (cdr rs))))
             (else 
                (let ((x (switch_same_row (cdr state) (cdr rs) line (- row 1))))
                   (list (cons (car state) (car x)) (cons (car rs) (cadr x)))
                )
             )
       )
    )
 )

; Si `state` est la représentation d'un état et si line et size sont des entiers positifs
; alors (find_gap line size) renvoie la liste contenant respectivement (line+i, j, size)
; avec i et j les coordonnées du trou dans l'état du taquin et size la taille du taquin.
; En particulier, (find_gap 1 1) renvoie la liste (i j size).
(define find_gap
    (lambda (state line size)
       (let ((x (find_gap_aux state 1)))
          (cond ((and (null? state) (= line size)) '(0 0 0))
                ((and (null? state) (not (= line size))) (list (- size 1)))
                ((> x 0) (append (list line x) (find_gap (cdr ls) line (+ size 1))))
                (else (find_gap (cdr state) (+ line 1) (+ size 1)))
          )
       )
    )
 )
 
; Si `state_line` est la représentation d'une ligne d'un état et si row est un entier positif
; alors (find_gap_aux state_line row) renvoie 0 si "state_line" ne contient pas le trou et
; renvoie row + n avec n la position du trou dans "state_line".
; En particulier, (find_gap_aux state 1) renvoie la position du trou dans "state_line".
 (define find_gap_aux 
    (lambda (state row)
       (cond ((null? state) 0)
             ((list? (car state)) (find_gap_aux (car state) row))
             ((equal? (car state) 'x) row)
             (else (find_gap_aux (cdr state) (+ row 1)))
       )
    )
 )


;;;;;;;;;;;;;;;;;;;;taquin acc state
; (taquin-acc-state? state) renvoie vrai si et seulement si `state` est la
; représentation de l'état accepteur
 (define taquin-acc-state?
    (lambda (state) 
        (taquin-acc-state-aux state 1 1 (sup-list-length state 0))))

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

; La fonction taquin-make-state prend en entrée une liste de N
; listes. La i-ème sous-liste représente la i-ème ligne du taquin, en
; commençant à compter depuis le haut. Les nombres sont numérotés à partir de 1
; et le trou est symbolisé par le symbole 'x.
; La fonction renvoie la représentation de l'état correspondant.
(define (taquin-make-state ls)
    ls
)