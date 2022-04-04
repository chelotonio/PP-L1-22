#lang racket

(define mi-filtro
 (lambda (pred lista)
   (cond
     [(null? lista) null]
     [(pred(car lista)) ;; Si predicado retorna True, ejecuto la siguiente línea. Caso contrario, ejecutar else
      (cons (car lista) (mi-filtro pred (cdr lista)))]
     [else (mi-filtro pred (cdr lista))])))

(define pertenece?
  (lambda (x lst)
    (cond
      [(null? lst) #f]
      [(eq? x (car lst)) #t]
      [else
       (pertenece? x (cdr lst))])))

(map eq? '(1 4 3) '(1 2 3))

(define elemento-comun?
   (lambda (lista)
     (cond
       [(null? lista) #f]
       ; Condición [... #t]
       [else
        (elemento-comun? (cdr lista))])))

(define dobble-par?
   (lambda (carta1 carta2)
  (map eq? carta1 carta2)))