#lang racket
(provide rp-solve)
(provide rp-solve-heuristic)

;si 'sits' est un ensamble de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est un mot qui a été apliqué sur une situation initialle,
;   le deuxiemme élement est l'état actuelle du puzzle, et le troixiéme élément est un ensamble d'etats déja visité),
;'answers' une liste de mots renversé qui menent l'état initiale du puzzle a l'état accepteur,
;et 'acc-state?' le prédcat d'acceptation d'u état du puzzle.
;find-answers retourne la liste des mots menant de l'état initial à l'état accepteru aillent aité trouvé parmis 'sits' et 'answers'
;
(define (find-answers sits answers acc-state?)
    (if (set-empty? sits) answers
        (if (acc-state? (cadr(set-first sits))) (find-answers (set-rest sits) (cons (car(set-first sits)) answers) acc-state?)
            (find-answers (set-rest sits) answers acc-state?)
        )
    )
)
;si 'sits' est un ensamble de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est un mot qui a été apliqué sur une situation initialle,
;   le deuxiemme élement est l'état actuelle du puzzle, et le troixiéme élément est un ensamble d'etats déja visité),
;'answers' une liste de mots renversé qui menent l'état initiale du puzzle a l'état accepteur n'aillent pas encore été affiché,
;'acc-state?' le prédcat d'acceptation d'u état du puzzle,
;et 'adj' la fonction d'adjacence du puzzle.
;solver-aux retourne une paire pointé ou le car est un mot de 'answers' renversé et
;le cdr et une procédure pour la quelle son évaluation retourne a nouveau une paire pointé du même type avec une solution différente au précedantes 
(define (solver-aux sits adj acc-state? answers)
    (if (and (set-empty? sits)(null? answers)) '()
        (let ((new-answers (find-answers sits answers acc-state? )))
          (if (null? new-answers) (solver-aux (make-move sits adj acc-state?) adj acc-state? new-answers)
              (cons (reverse(car new-answers)) (lambda () (solver-aux (remove-answers sits acc-state?) adj acc-state? (cdr new-answers))))
          )
        )
    )
)
;si 'sits' est un ensamble de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est un mot qui a été apliqué sur une situation initialle,
;   le deuxiemme élement est l'état actuelle du puzzle, et le troixiéme élément est un ensamble d'etats déja visité),
;et 'acc-state?' le prédcat d'acceptation d'u état du puzzle.
;remove-answers retourne l'ensamble des situations 'sits' ou les situations contenant des états accepteur ont été éliminé.
(define (remove-answers sits acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state? (cadr(set-first sits))) (remove-answers (set-rest sits) acc-state?)
            (set-add (remove-answers (set-rest sits) acc-state?) (set-first sits))
        )
    )
)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle,
; (rp-solve s adj acc-state?) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés par ordre de longueur.
(define (rp-solve s adj acc-state?)
    (lambda ()
        (solver-aux (set(list '() s '())) adj acc-state? '())
    )
)

;si 'sits' est un ensamble de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est un mot qui a été apliqué sur une situation initialle,
;   le deuxiemme élement est l'état actuelle du puzzle, et le troixiéme élément est un ensamble d'etats déja visité),
;'acc-state?' le prédcat d'acceptation d'u état du puzzle,
;et 'adj' la fonction d'adjacence du puzzle.
;make-move retourne un ensamble des situations résultantes de l'aplication de adj sur chaque élements de sits
(define (make-move sits adj acc-state?)
    (if (set-empty? sits) (set )
        (if (acc-state? (cadr(set-first sits))) (make-move (set-rest sits) adj acc-state?)
            (set-union (make-move-aux (set-first sits) (adj(cadr(set-first sits))))(make-move (set-rest sits) adj acc-state?))    
        )
    )
)
;si 'old-state' est une situation d'un puzzle régulier
;   (une situation est une liste ou le premier élement est un mot qui a été apliqué sur une situation initialle,
;   le deuxiemme élement est l'état actuelle du puzzle, et le troixiéme élément est un ensamble d'etats déja visité),
;et 'new-states' est une liste de paires pointé contenant tout les états adjacents a l'état dans old-state et le mouvemant menant a cet état
;make-move-aux retourne un ensamble de situations adjacentes a old-state valides et non membres des états pŕecédants de old-state
;
(define (make-move-aux old-state new-states)
    (if (null? new-states) (set )
        (if (equal? 'sink (cdar new-states)) (make-move-aux old-state (cdr new-states))
            (if (set-member? (caddr old-state)(cdar new-states)) (make-move-aux old-state (cdr new-states))
                (set-add (make-move-aux old-state (cdr new-states)) (make-new-state old-state (car new-states)))
            )        
        )
    )
)
;si 'old-state' est une situation d'un puzzle régulier
;   (une situation est une liste ou le premier élement est un mot qui a été apliqué sur une situation initialle,
;   le deuxiemme élement est l'état actuelle du puzzle, et le troixiéme élément est un ensamble d'etats déja visité),
;et new-pair est une paire pointé ou le car est une lettre de l'alphabet du puzzle et le cdr est un état adjacent a l'état de old-sate
;make-new-state retourne une situation ou le mot est la letre de new-pair concatené au mot de old-state, l'état est l'état de new-pair,
;et lensamble de old-state au quelle l'état de old-state a été rajouté.
(define (make-new-state old-state new-pair)
    (list (cons (car new-pair)(car old-state)) (cdr new-pair) (set-add (caddr old-state) (cadr old-state)))
)


;;;;;;;;;;;;;;;;;;;; heuristic

;si new-sits et old-sits sont des listes de situations d'un puzzle régulier, avec old-sits trié en ordre corisant d'heuristic
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité),
;merge-sits renvoi la liste trié de situations produite par la concaténation de new-sits et old-sits.
(define (merge-sits new-sits old-sits)
    (if(null? new-sits) old-sits
        (merge-sits (cdr new-sits)(merge-aux(car new-sits) old-sits) )    
    )
)
;si new-sit est une situation d'un puzzle régulier et old-sits une liste de situations d'un puzzle régulier
;trié par ordre croissant d'heurisic,
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité)
;merge-aux renvoi la liste de situations trié par ordre croissant produte de l'insertion de new-sit dans old-sits.
(define (merge-aux new-sit old-sits)
    (if (null? old-sits) (list new-sit)
        (if(< (car new-sit) (caar old-sits)) (cons new-sit old-sits)
            (cons (car old-sits) (merge-aux new-sit (cdr old-sits)))
        )    
    )     
)
;si 'sits' est une liste de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité),
;'acc-state?' le prédcat d'acceptation d'u état du puzzle,
;`heuristic` est une fonction d'heuristic pour ce puzzle,
;et 'adj' la fonction d'adjacence du puzzle.
;make-move-heu retourne la liste des situations résultantes de l'aplication de adj sur le premier élement de sits.
(define (make-move-heu sits adj acc-state? heuristic)
    (if (null? sits) '() 
        (merge-sits (make-move-heu-aux (car sits) (adj(caddar sits)) heuristic) (cdr sits))
        
    )    
)
;si 'old-state' est une situation d'un puzzle régulier
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité),
;`heuristic` est une fonction d'heuristic pour ce puzzle,
;et 'new-states' est une liste de paires pointé contenant tout les états adjacents a l'état dans old-state et le mouvemant menant à cet état
;make-move-heu-aux retourne une liste de situations adjacentes à old-state valides et non membres des états pŕecédants de old-state
;
(define (make-move-heu-aux old-sit new-states heuristic)
    (if (null? new-states) '()
        (if(equal? 'sink (cdar new-states)) (make-move-heu-aux old-sit (cdr new-states) heuristic)
            (if(set-member? (cadddr old-sit) (cdar new-states)) (make-move-heu-aux old-sit (cdr new-states) heuristic)
                (cons  (make-new-state-heu old-sit (car new-states) heuristic) (make-move-heu-aux old-sit (cdr new-states) heuristic))
            )
        )
    )
)
;si 'old-state' est une situation d'un puzzle régulier
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité),
;`heuristic` est une fonction d'heuristic pour ce puzzle,
;et new-pair est une paire pointé ou le car est une lettre de l'alphabet du puzzle et le cdr est un état adjacent a l'état de old-sate
;make-new-state retourne une situation ou le mot est la premiere letre de new-pair concatené au mot de old-state, l'état est l'état de new-pair,
;et l'ensamble de old-state au quelle l'état de old-state a été rajouté.
(define (make-new-state-heu old-sit new-pair heuristic)
    (list (heuristic (cdr new-pair)) (cons (car new-pair)(cadr old-sit))(cdr new-pair) (set-add(cadddr old-sit)(caddr old-sit)))
)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle et
; `heuristic` est une fonction d'heuristic pour ce puzzle,
; (rp-solve-heuristic s adj acc-state? heuristic) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés dans l'ordre de l'heuristic
(define (rp-solve-heuristic s adj acc-state? heuristic )
    (lambda ()
        (solver-heu-aux (list(list (heuristic s) '() s (set ))) '() adj acc-state? heuristic)
    )
)

;si 'sits' est une liste de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité),
;'acc-state?' le prédcat d'acceptation d'u état du puzzle,
;`heuristic` est une fonction d'heuristic pour ce puzzle,
;et 'adj' la fonction d'adjacence du puzzle.
;solver-heu-aux retourne une paire pointé ou le car est un mot de 'answers' renversé et
;le cdr et une procédure pour la quelle son évaluation retourne a nouveau une paire pointé du même type avec une solution différente au précedantes
(define (solver-heu-aux sits answers adj acc-state? heuristic)
    (if(and (null? answers)(null? sits)) '()
        (let ((new-answers (find-answers-heu sits answers)))
            (if (null? new-answers) (solver-heu-aux (make-move-heu sits adj acc-state? heuristic) new-answers adj acc-state? heuristic)
                (cons (reverse(car new-answers)) (lambda () (solver-heu-aux (remove-answers-heu sits) (cdr new-answers) adj acc-state? heuristic)))
            )
        )
    )
)
;si 'sits' est une liste de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité),
;'answers' une liste de mots renversé qui menent l'état initiale du puzzle a l'état accepteur
;find-answers-heu retourne la liste des mots menant de l'état initial à l'état accepteru aillent aité trouvé parmis 'sits' et 'answers'
(define (find-answers-heu sits answers)
    (if (null? sits) answers
        (if(>(caar sits) 0) answers
             (find-answers-heu (cdr sits) (cons (cadar sits) answers))
        )
    )
)
;si 'sits' est une liste de situations d'un puzzle régulier
;   (une situation est une liste ou le premier élement est la valeur heuristic associé a l'état du puzzle,
;   le deuxiemme élement un mot qui a été apliqué sur une situation initialle,
;   le troixiemme élement est l'état actuelle du puzzle,
;   et le quatriémme élément est un ensamble d'etats déja visité)
;remove-answers-heu retourne la liste des situations 'sits' ou les situations contenant des états accepteur ont été éliminé.
(define (remove-answers-heu sits)
    (if (null? sits) '()
            (if(= (caar sits) 0)  (remove-answers-heu (cdr sits))
                sits
            )
    )
)