; (1) Testar se um elemento é membro de uma lista

(define (member? elem lst)
  (cond ((null? lst) #f)
        ((equal? elem (car lst)) #t)
        (else (member? elem (cdr lst)))))

(display (member? 3 '(1 2 3 4)))
(newline)

; (2) Calcular o tamanho de uma lista
(define (length' lst)
  (if (null? lst)
      0
      (+ 1 (length' (cdr lst)))))

(display (length '(1 2 3 4 5)))
(newline)

; (3) Calcular a soma dos elementos de uma lista
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(display (sum-list '(1 2 3 4)))
(newline)

; (4) Calcular o produto dos elementos de uma lista
(define (product-list lst)
  (if (null? lst)
      1
      (* (car lst) (product-list (cdr lst)))))

(display (product-list '(1 2 3 4 5 0)))
(newline)

; (5) Reversão de lista
(define (reverse' lst)
  (define (reverse-aux lst acc)
    (if (null? lst)
        acc
        (reverse-aux (cdr lst) (cons (car lst) acc))))
  (reverse-aux lst '()))

(display (reverse '(1 2 3 4 5)))
(newline)

; (6) Testar se duas listas são iguais
(define (equal-lists? lst1 lst2)
  (cond ((and (null? lst1) (null? lst2)) #t)
        ((or (null? lst1) (null? lst2)) #f)
        ((equal? (car lst1) (car lst2)) (equal-lists? (cdr lst1) (cdr lst2)))
        (else #f)))

(display (equal-lists? '(1 2 3) '(1 2 4)))
(newline)

; (7) Concatenação de duas listas
(define (append' lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append' (cdr lst1) lst2))))

(display (append '(1 2 3) '(4 5 6)))
(newline)

; (8) Intersecção de duas listas
(define (intersection lst1 lst2)
  (cond ((null? lst1) '())
        ((member? (car lst1) lst2) (cons (car lst1) (intersection (cdr lst1) lst2)))
        (else (intersection (cdr lst1) lst2))))

(display (intersection '(1 2 3 4) '(3 4 5 6)))
(newline)

; (9) Método de ordenação (quicksort)
(define (quicksort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append (quicksort (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

(display (quicksort '(8 9 4 2 4 1 0 6 5 7)))
(newline)