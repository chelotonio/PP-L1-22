#lang racket

; TDA CardsSet
; Representación computacional de la generación de cartas
; con sus respectivos símbolos, respetando las restricciones
; correspondientes del juego Dobble.

; Representación: Listas

(define numeros
  (list 1 2 3 4 5 6 7 8)

; Elements...
; (list 1 2 3 4 5 6 7)
; (list "A" "B" "C" "D" "E" "F" "G")
; (list "Arból" "Manzana" "Plátano" "Zorro" "Lana" "Cama" "Silla")
        

; ((Elements) 2 3 randomFn) || ((Elements) 3 7 randomFn)
; '((1 2 3) (1 4 5) ())

; Función que...
; Dominio:
; Recorrido:
(define buscar-elemento
  (lambda (i n lista)
    (cond
      ((null? lista) null)
      ((eq? i n) (car lista))
      (else (buscar-elemento (+ i 1) n (cdr lista))))))

(define acotar-cartas
  (lambda (maxC list i)
    (cond
      ((null? list) null)
      ((> i maxC) null)
      (else (cons (car list) (acotar-cartas maxC (cdr list) (+ i 1)))))))  

; Función que...
; Dominio:
; Recorrido:
(define n2-cartas-2
          (lambda (k j i numE Elements-entrada)
            (cond
          ((= k 1) (cons (buscar-elemento 1 (+ i 1) Elements-entrada) (n2-cartas-2 (+ k 1) j i numE Elements-entrada)))
          ((> k numE 1) null)
          (else (cons
  (buscar-elemento 1 (+ (+ (* (- numE 1) (- k 2)) (- numE 1) 2) (modulo (- (+ (* (- i 1) (- k 2))  j) 1) (- numE 1))) Elements-entrada)               
                 (n2-cartas-2 (+ k 1) j i numE Elements-entrada))))))

; Función que...
; Dominio:
; Recorrido:
(define n2-cartas
      (lambda (i j numE Elements-entrada)
        (cond
          ((> i (- numE 1)) baraja)
          ((> j (- numE 1)) (n2-cartas (+ i 1) 1 numE Elements-entrada))
          (else (agregar-carta-baraja (n2-cartas i (+ j 1) numE Elements-entrada) (n2-cartas-2 1 j i numE Elements-entrada))))))

; Función que...
; Dominio:
; Recorrido:
(define n-cartas-2
          (lambda (k j numE Elements-entrada)
            (cond
          ((= k 1) (cons (buscar-elemento 1 1 Elements-entrada) (n-cartas-2 (+ k 1) j numE Elements-entrada)))
          ((> k numE 1) null)
          (else (cons (buscar-elemento 1 (+ (* (- numE 1) j) (+ k 0)) Elements-entrada) (n-cartas-2 (+ k 1) j numE Elements-entrada))))))

; Función que...
; Dominio:
; Recorrido:
(define n-cartas
      (lambda (j numE Elements-entrada)
        (cond
          ((> j (- numE 1)) baraja)
          (else (agregar-carta-baraja (n-cartas (+ j 1) numE Elements-entrada) (n-cartas-2 1 j numE Elements-entrada))))))

; Función que...
; Dominio:
; Recorrido:
(define cardsSet
  (lambda (Elements-entrada numE maxC rndFn)
    (define primera-carta
      (lambda (i)
        (cond
          ((> i numE) null)
          (else (cons (buscar-elemento 1 i Elements-entrada) (primera-carta (+ i 1))))
          )))

    (define baraja-final
      (append (list (primera-carta 1)) (append (n-cartas 1 numE Elements-entrada) (n2-cartas 1 1 numE Elements-entrada))))

    (cond
      ((> maxC 0) (acotar-cartas maxC baraja-final 1))
      (else baraja-final))

    ))

; (define baraja
(define baraja
  null)
  
  ; (cardsSet (Elements) 2 3 randomFn)
  ; '((1 2) (1 3) (2 3))
  ; '((1 2 3) (1 4 5) (1 6 7) (2 4 6) (2 5 7) (3 4 7) (3 5 6))
  
; (dobble? baraja
; Verificar que los elementos en una carta sean distintos,
; y que para cada carta existe un solo elemento en común
; para cualquier otra carta de la baraja.
; Crear función que revise toda la lista (recursión)

(define dobble-par?
  (lambda (carta1 carta2 )
    (map eq? carta1 carta2)))

; Función que...
; Entrada:
; Salida:
(define dobble?
  (lambda (cardsSet-entrada)
    (#t)))

; Función que...
; Entrada:
; Salida:
(define numCards
  (lambda (cardsSet-entrada)
    ))

; Función que...
; Entrada:
; Salida:
(define nthCard
  (lambda (cardsSet-entrada carta-seleccionada)
    (carta)))

; Función que...
; Entrada:
; Salida:
(define findTotalCards
  (lambda (carta-entrada)
    ))

; Función que...
; Entrada:
; Salida:
(define requiredElements
  (lambda (carta-entrada)
    ))

; Función que...
; Entrada:
; Salida:
(define missingCards
  (lambda (cardsSet-entrada)
    (cardsSet-salida)))

; Función que...
; Entrada:
; Salida:
(define cardsSet->string
  (lambda (cardsSet-entrada)
    (cardsSet-string)))

