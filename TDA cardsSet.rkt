#lang racket

; TDA CardsSet
; Representación computacional de la generación de cartas
; con sus respectivos símbolos, respetando las restricciones
; correspondientes del juego Dobble.

; Representación: Listas

; Posibles Elements-entrada
; '(1 2 3 4 5 6 7)
; '("A" "B" "C" "D" "E" "F" "G")
; '("Arból" "Manzana" "Plátano" "Zorro" "Lana" "Cama" "Silla")

; TDA baraja: Lista vacia donde se almacenarán todas las cartas del cardsSet.
(define baraja
  null)

; ______________ TDA CardsSet - Constructor ______________

; Función que agrega una carta a a una baraja determinada.
; Dominio: baraja-entrada (list) X  carta (list)
; Recorrido: list of lists
(define agregar-carta-baraja
  (lambda (baraja-entrada carta)
    (append baraja-entrada (list carta))))

; Función que inserta el enésimo elemento de una lista.
; Dominio: i (int) X n (int) X lista (list) 
; Recorrido: int O str
(define buscar-elemento
  (lambda (i n lista)
    (cond
      ((null? lista) null)
      ((eq? i n) (car lista))
      (else (buscar-elemento (+ i 1) n (cdr lista))))))

; Función que limita la cantidad de cartas a generar.
; Dominio: maxC (int) X lista (list) X i (int) 
; Recorrido: list
(define acotar-cartas
  (lambda (maxC lista i)
    (cond
      ((null? lista) null)
      ((> i maxC) null)
      (else (cons (car lista) (acotar-cartas maxC (cdr lista) (+ i 1)))))))  

; Función que genera cada carta de las primeras n (numE - 1) cuadrado cartas de la baraja.
; Dominio: k (int) X j (int) X i (int) X numE (int) X Elements-entrada (list)
; Recorrido: list
(define n2-cartas-2
          (lambda (k j i numE Elements-entrada)
            (cond
          ((= k 1) (cons (buscar-elemento 1 (+ i 1) Elements-entrada) (n2-cartas-2 (+ k 1) j i numE Elements-entrada)))
          ((> k numE 1) null)
          (else (cons
  (buscar-elemento 1 (+ (+ (* (- numE 1) (- k 2)) (- numE 1) 2) (modulo (- (+ (* (- i 1) (- k 2))  j) 1) (- numE 1))) Elements-entrada)               
                 (n2-cartas-2 (+ k 1) j i numE Elements-entrada))))))

; Función que almacena las ultimas n (numE - 1) cuadrado cartas de la baraja.
; Dominio: i (int) X j (int) X numE (int) X Elements-entrada (list)
; Recorrido: list of lists
(define n2-cartas
      (lambda (i j numE Elements-entrada)
        (cond
          ((> i (- numE 1)) baraja)
          ((> j (- numE 1)) (n2-cartas (+ i 1) 1 numE Elements-entrada))
          (else (agregar-carta-baraja (n2-cartas i (+ j 1) numE Elements-entrada) (n2-cartas-2 1 j i numE Elements-entrada))))))

; Función que genera cada carta de las primeras n (numE - 1) cartas de la baraja.
; Dominio: k (int) X j (int) X numE (int) X Elements-entrada (list)
; Recorrido: list
(define n-cartas-2
          (lambda (k j numE Elements-entrada)
            (cond
          ((= k 1) (cons (buscar-elemento 1 1 Elements-entrada) (n-cartas-2 (+ k 1) j numE Elements-entrada)))
          ((> k numE 1) null)
          (else (cons (buscar-elemento 1 (+ (* (- numE 1) j) (+ k 0)) Elements-entrada) (n-cartas-2 (+ k 1) j numE Elements-entrada))))))

; Función que almacena las primeras n (numE - 1) cartas de la baraja.
; Dominio: j (int) X numE (int) X Elements-entrada (list)
; Recorrido: list of lists
(define n-cartas
      (lambda (j numE Elements-entrada)
        (cond
          ((> j (- numE 1)) baraja)
          (else (agregar-carta-baraja (n-cartas (+ j 1) numE Elements-entrada) (n-cartas-2 1 j numE Elements-entrada))))))

; Función constructora de conjuntos válidos de cartas para el juego Dobble.
; Dominio: Elements-entrada (list) X numE (int) X maxC (int) X rndFn (Fn)
; Recorrido: cardsSet (list of lists)
(define cardsSet
  (lambda (Elements-entrada numE maxC rndFn)
    (define primera-carta
      (lambda (i)
        (cond
          ((> i numE) null)
          (else (cons (buscar-elemento 1 i Elements-entrada) (primera-carta (+ i 1))))
          )))
    ; Se juntan la primera carta,las siguientes n cartas y las últimas n cuadrado cartas en la baraja.
    (define baraja-final
      (append (list (primera-carta 1)) (append (n-cartas 1 numE Elements-entrada) (n2-cartas 1 1 numE Elements-entrada))))
    ; Si se define un número maximo mayor a cero se acota la cantidad de cartas a numE.
    (cond
      ((> maxC 0) (acotar-cartas maxC baraja-final 1))
      (else baraja-final))
    ))

; ______________ TDA CardsSet - dobble? ______________

; Función que cambia valores booleanos por valores enteros (#t -> 1, #f -> 0).
(define cambiar-boolean
  (lambda (lista)
    (cond
      ((null? lista) null)
      ((car lista) (cons 1 (cambiar-boolean (cdr lista))))
      (else (cons 0 (cambiar-boolean (cdr lista)))))))

; Función que compara un par de cartas y comprueba que solo haya un elemento en común.
(define es-par-dobble?
  (lambda (carta1 carta2)
    (cond
      ((eq? 1 (contar-1s (cambiar-boolean (map eq? carta1 carta2)) 0)))
      (else #f))))

; Función que cuenta la cantidad de elementos en común de un par de cartas.
(define contar-1s
  (lambda (lista i)
    (cond
      ((null? lista) i)
      ((eq? (car lista) 1) (contar-1s (cdr lista) (+ i 1)))
      (else (contar-1s (cdr lista) i)))))

; Función que cuenta la cantidad de cartas presentes en una baraja.
(define contar-baraja
  (lambda (lista i)
    (cond
      ((null? lista) i)
      (else (contar-baraja (cdr lista) (+ i 1))))))

; Función que devuelve falso si quedan más de 1 carta en la baraja, en caso contrario devuelve verdadero.
(define no-queda-par-de-cartas?
  (lambda (baraja)
    (cond
      ((< 1 (contar-baraja baraja 0)) #f)
      (else #t))))

; Función que permite verificar si el conjunto de cartas en el conjunto corresponden a un conjunto válido.
; Dominio: cardsSet (list)
; Recorrido: boolean
(define dobble?
  (lambda (cardsSet-entrada)
    (cond
      ((no-queda-par-de-cartas? cardsSet-entrada) #t)
      ((es-par-dobble? (car cardsSet-entrada) (car (cdr cardsSet-entrada))) (dobble? (cdr cardsSet-entrada)))
      (else #f))))

; ______________ TDA CardsSet - numCards ______________

; Función que...
; Entrada:
; Salida:
(define numCards
  (lambda (cardsSet-entrada)
    null))

; Función que...
; Entrada:
; Salida:
(define nthCard
  (lambda (cardsSet-entrada carta-seleccionada)
    (null)))

; Función que...
; Entrada:
; Salida:
(define findTotalCards
  (lambda (carta-entrada)
    null))

; Función que...
; Entrada:
; Salida:
(define requiredElements
  (lambda (carta-entrada)
    null))

; Función que...
; Entrada:
; Salida:
(define missingCards
  (lambda (cardsSet-entrada)
    (null)))

; Función que...
; Entrada:
; Salida:
(define cardsSet->string
  (lambda (cardsSet-entrada)
    (null)))

; Ejemplos de CardsSet.
; (cardsSet '(1 2 3 4 5 6 7 8 9 10 11 12 13) 4 10 0)
; (cardsSet '("Arból" "Manzana" "Plátano" "Zorro" "Lana" "Cama" "Silla") 3 10 0)
; (cardsSet '("A" "B" "C" "D" "E" "F" "G") 2 10 0)

; Ejemplos de dobble?.
; (dobble? (cardsSet '(1 2 3 4 5 6) 2 10 0))
; (dobble? '((1 2 3) (1 4 5) (1 6 7) (2 6 8) (2 7 9) (3 4 9)))
; (dobble? '((1 2) (1 3) (1 3))) ... Ejemplo donde coinciden dos cartas con más de un elemento en común.
; (dobble? '((1 2) (2 4) (5 6))) ... Ejemplo donde ninguna carta posee un elemento en común.
