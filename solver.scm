#lang racket
(provide rp-solve)
(provide rp-solve-heuristique)

;Une situation est une liste contenant respectivement un mot qui a été appliqué
;sur une situation initiale, l'état actuel du puzzle et un ensemble d'états 
;déjà visités.
;Si 'sits' est un ensemble de situations d'un puzzle régulier, 'answers' une 
;liste de mots renversés qui mènent l'état initial du puzzle à l'état accepteur 
;et 'acc-state?' le prédicat accepteur du puzzle, alors
;(find-answers sits answers acc-state?) retourne la liste des mots se trouvant 
;dans 'sits' et 'answers' et menant l'état initial à l'état accepteur.

(define (find-answers sits answers acc-state?)
    (if (set-empty? sits) answers
        (if (acc-state? (cadr(set-first sits))) (find-answers (set-rest sits) (cons (car(set-first sits)) answers) acc-state?)
            (find-answers (set-rest sits) answers acc-state?)
        )
    )
)

;Une situation est une liste contenant respectivement un mot qui a été appliqué
;sur une situation initiale, l'état actuel du puzzle et un ensemble d'états 
;déjà visités.
;Si 'sits' est un ensemble de situations d'un puzzle régulier,
;'answers' une liste de mots renversés qui mènent l'état initial du puzzle à 
;l'état accepteur et n'ayant pas encore été affichée,'acc-state?' le prédicat 
;accepteur du puzzle et 'adj' la fonction d'adjacence du puzzle, alors
;(solver-aux sits adj acc-state? answers) retourne une paire pointée où le car 
;est un mot de 'answers' renversé et le cdr est une procédure dont l'évaluation
;retourne à nouveau une paire pointée du même type avec une solution différente
;des précédentes.
(define (solver-aux sits adj acc-state? answers)
    (if (and (set-empty? sits)(null? answers)) '()
        (let ((new-answers (find-answers sits answers acc-state? )))
          (if (null? new-answers) (solver-aux (make-move sits adj acc-state?) adj acc-state? new-answers)
              (cons (reverse(car new-answers)) (lambda () (solver-aux (remove-answers sits acc-state?) adj acc-state? (cdr new-answers))))
          )
        )
    )
)

;Une situation est une liste contenant respectivement un mot qui a été appliqué 
;sur une situation initiale, l'état actuel du puzzle et un ensemble d'états 
;déjà visités.
;Si 'sits' est un ensemble de situations d'un puzzle régulier
;et 'acc-state?' le prédicat accepteur d'un état du puzzle, alors
;(remove-answers sits acc-states?) retourne l'ensemble des situations 'sits' où 
;les situations contenant des états accepteurs ont été éliminés.
(define (remove-answers sits acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state? (cadr(set-first sits))) (remove-answers (set-rest sits) acc-state?)
            (set-add (remove-answers (set-rest sits) acc-state?) (set-first sits))
        )
    )
)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le prédicat accepteur du puzzle,
; (rp-solve s adj acc-state?) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés par ordre de longueur.
(define (rp-solve s adj acc-state?)
    (lambda ()
        (solver-aux (set(list '() s '())) adj acc-state? '())
    )
)

;Une situation est une liste contenant respectivement un mot qui a été appliqué 
;sur une situation initiale, l'état actuel du puzzle et un ensemble d'états 
;déjà visités.
;Si 'sits' est un ensemble de situations d'un puzzle régulier,
;'acc-state?' le prédicat accepteur du puzzle et 'adj' la fonction d'adjacence 
;du puzzle, alors (make-move sits adj acc-state?) retourne un ensemble des 
;situations résultantes de l'application de la fonction 'adj' sur chaque 
;élément de 'sits'.
(define (make-move sits adj acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state? (cadr(set-first sits))) (make-move (set-rest sits) adj acc-state?)
            (set-union (make-move-aux (set-first sits) (adj(cadr(set-first sits))))(make-move (set-rest sits) adj acc-state?))    
        )
    )
)

;Une situation est une liste contenant respectivement un mot qui a été appliqué 
;sur une situation initiale, l'état actuel du puzzle et un ensemble d'états 
;déjà visités.
;si 'old-state' est une situation d'un puzzle régulier
;et 'new-states' une liste de paires pointées dont les cdr sont des
;états adjacents à l'état de 'old-state' et dont les car sont le mouvement menant à
;cet état, alors (make-move-aux old-state new-states) retourne un ensemble de 
;situations valides, adjacentes à 'old-state' et n'étant pas dans l'ensemble 
;d'états déjà visités de 'old-state'.
(define (make-move-aux old-state new-states)
    (if (null? new-states) (set )
        (if (equal? 'sink (cdar new-states)) (make-move-aux old-state (cdr new-states))
            (if (set-member? (caddr old-state)(cdar new-states)) (make-move-aux old-state (cdr new-states))
                (set-add (make-move-aux old-state (cdr new-states)) (make-new-state old-state (car new-states)))
            )        
        )
    )
)

;Une situation est une liste contenant respectivement un mot qui a été appliqué 
;sur une situation initiale, l'état actuel du puzzle et un ensemble d'états 
;déjà visités.
;Si 'old-state' est une situation d'un puzzle régulier
;et 'new-pair' une paire pointée où le car est une lettre de l'alphabet du
;puzzle et le cdr est un état adjacent à l'état de 'old-state', alors



;(make-new-state old-state new-pair) retourne une situation où le mot est la
;lettre de 'new-pair' concatenée au mot de 'old-state' et où l'état est celui adjacent de
;'new-pair' et l'ensemble de 'old-state' auquel l'état de 'old-state' a été rajouté.
(define (make-new-state old-state new-pair)
    (list (cons (car new-pair)(car old-state)) (cdr new-pair) (set-add (caddr old-state) (cadr old-state)))
)


;;;;;;;;;;;;;;;;;;;; heuristique
;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'new-sits' et 'old-sits' sont des listes de situations d'un puzzle régulier
;avec 'old-sits' trié en ordre croissant d'heuristique, alors
;(merge-sits new-sits old-sits) renvoie la liste triée de situations produites 
;par la fusion de 'new-sits' et 'old-sits'.
(define (merge-sits new-sits old-sits)
    (if(null? new-sits) old-sits
        (merge-sits (cdr new-sits)(merge-aux(car new-sits) old-sits) )    
    )
)

;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'new-sit' est une situation d'un puzzle régulier et 'old-sits' une liste de
;situations d'un puzzle régulier triée par ordre croissant d'heuristique, alors
;(merge-aux new-sit old-sit) renvoie la liste triée de la fusion de 'new-sits' et 'old-sits'.
(define (merge-aux new-sit old-sits)
    (if (null? old-sits) (list new-sit)
        (if(< (car new-sit) (caar old-sits)) (cons new-sit old-sits)
            (cons (car old-sits) (merge-aux new-sit (cdr old-sits)))
        )    
    )     
)


;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'sits' est une liste de situations d'un puzzle régulier, 'acc-state?' le
;prédicat accepteur du puzzle, `heuristique` une fonction d'heuristique 
;pour ce puzzle et 'adj' la fonction d'adjacence du puzzle, alors
;(make-move-heu sits adj acc-state? heuristique) retourne la liste des
;situations résultantes de l'application de 'adj' sur le premier élément 
;'de sits'.
(define (make-move-heu sits adj acc-state? heuristique)
    (if (null? sits) '() 
        (merge-sits (make-move-heu-aux (car sits) (adj(caddar sits)) heuristique) (cdr sits))
    )    
)


;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'old-state' est une situation d'un puzzle régulier, `heuristique` est une
; fonction d'heuristique pour ce puzzle et 'new-states' est une liste de paires 
;pointées contenant tous les états adjacents à l'état de 'old-state' et le
;mouvement menant à cet état, alors
;(make-move-heu-aux old-sit new-states heuristique) retourne une liste de
;situations valides, adjacentes à 'old-state' et n'étant pas dans l'ensemble 
;d'états déjà visités de 'old-state'.
(define (make-move-heu-aux old-sit new-states heuristique)
    (if (null? new-states) '()
        (if(equal? 'sink (cdar new-states)) (make-move-heu-aux old-sit (cdr new-states) heuristique)
            (if(set-member? (cadddr old-sit) (cdar new-states)) (make-move-heu-aux old-sit (cdr new-states) heuristique)
                (cons  (make-new-state-heu old-sit (car new-states) heuristique) (make-move-heu-aux old-sit (cdr new-states) heuristique))
            )
        )
    )
)
;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'old-state' est une situation d'un puzzle régulier, `heuristique` est une fonction 
;d'heuristique pour ce puzzle et new-pair est une paire pointée où le car est une lettre
;de l'alphabet du puzzle et le cdr est un état adjacent à l'état de 'old-sate', alors 
;(make-new-state old-sit new-pair heuristique) retourne une situation où le mot est la 




;première lettre de 'new-pair' concatenée au mot de 'old-state' et où l'état est celui de
; 'new-pair' et l'ensemble est l'ensemble de 'old-state' auquel l'état de old-state a été 
;rajouté.
(define (make-new-state-heu old-sit new-pair heuristique)
    (list (heuristique (cdr new-pair)) (cons (car new-pair)(cadr old-sit))(cdr new-pair) (set-add(cadddr old-sit)(caddr old-sit)))
)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le prédicat accepteur de ce puzzle et
; `heuristique` est une fonction d'heuristique pour ce puzzle,
; (rp-solve-heuristique s adj acc-state? heuristique) est un itérateur paresseux de 
; solutions du puzzle. Les mots du langage sont donnés dans l'ordre de l'heuristique
(define (rp-solve-heuristique s adj acc-state? heuristique )
    (lambda ()
        (solver-heu-aux (list(list (heuristique s) '() s (set ))) '() adj acc-state? heuristique)
    )
)

;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'sits' est une liste de situations d'un puzzle régulier, 'acc-state?' le prédicat 
;accepteur du puzzle, `heuristique` est une fonction d'heuristique pour ce puzzle et 
;'adj' la fonction d'adjacence du puzzle, alors
;(solver-heu-aux sits answers adj acc-state? heuristique) retourne une paire pointée où 
;le car est un mot de 'answers' renversé et le cdr et une procédure dont l'évaluation 
;retourne à nouveau une paire pointée du même type avec une solution différente des précédentes
(define (solver-heu-aux sits answers adj acc-state? heuristique)
    (if(and (null? answers)(null? sits)) '()
        (let ((new-answers (find-answers-heu sits answers)))
            (if (null? new-answers) (solver-heu-aux (make-move-heu sits adj acc-state? heuristique) new-answers adj acc-state? heuristique)
                (cons (reverse(car new-answers)) (lambda () (solver-heu-aux (remove-answers-heu sits) (cdr new-answers) adj acc-state? heuristique)))
            )
        )
    )
)

;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'sits' est une liste de situations d'un puzzle régulier, 'answers' une liste de mots 
;renversés qui menent l'état initial du puzzle à l'état accepteur, alors 
;(find-answers-heu sits answers) retourne la liste des mots se trouvant dans 'sits' et 
;'answers' et menant l'état initial à l'état accepteur.
(define (find-answers-heu sits answers)
    (if (null? sits) answers
        (if(>(caar sits) 0) answers
             (find-answers-heu (cdr sits) (cons (cadar sits) answers))
        )
    )
)

;Une situation est une liste contenant respectivement la valeur heuristique
;associée à l'état du puzzle, un mot qui a été appliqué sur une situation
;initiale, l'état actuel du puzzle et un ensemble d'états déja visités.
;Si 'sits' est une liste de situations d'un puzzle régulier, alors 
;(remove-answers-heu sits) retourne la liste des situations 'sits' où les situations 
;contenant des états accepteurs ont été éliminés.
(define (remove-answers-heu sits)
    (if (null? sits) '()
            (if(= (caar sits) 0)  (remove-answers-heu (cdr sits))
                sits
            )
    )
)