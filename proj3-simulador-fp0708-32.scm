(require (lib "include.ss"))
(include "proj3-margarida-fp0708-32.scm")
(include "proj3-planeta-fp0708-32.scm")
(include "proj3-aux-fp0708-32.scm")
(include "proj3-posicao-fp0708-32.scm")
(require "gerador.zo")
(require "tokenize.zo")

(define erro-de-utilizacao "Erro de utilização.") ;constante que contém a string que será devolvida na má invocação do simulador


;TAI - Simulador
;A Representação interna é constituida por um vector com duas posições em que a primeira posição contém a representação interna de um
;planeta e o segundo elemento é constituido por outro vector com os parâmetros de uma simulação, por esta ordem :
;seed, init, steps, trace, albedo, lumino, densidade, vizinhanca, mutate.


;construtor
;nova_simulacao : Planeta X Lista → Simulacao ∪ String
(define (nova-simulacao planeta args)
  
  (define v_lidos (vector 0 0 0 0 0 0 0 0 0)) ;vector que garante que não estão passados parametros repetidos
  ;ou seja, para uma simulacao ser valida, cada parametro so pode ser passado no maximo 1 vez
  
  (define por_defeito (vector 0 0 1 "off" 0.5 "lumino" 0 1 0)) ;valores por defeito para os parametros do simula  
  
  ;altera o valor da semente de uma simulação
  (define (altera_seed elem)
    (if (and (integer? elem)(<= elem (- (expt 2 31) 1)) (>= elem 0)) ;validação do valor da semente
        (begin (vector-set! por_defeito 0 elem) ; altera valor da semente
               (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 1))) ;nº de vezes que o parametro "-seed" ocorre
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2)))) 
  ;se o valor da semente não for válido, o vector que nos informa quantas vezes cada parametro é passado, irá ficar inválido, 
  ;afim de no final ser devolvido a mensagem do erro de utilização. O mesmo é similar para os outros 8 procedimentos em baixo 
  ;em que cada um altera os valores dos outros parametros.
  
  ;altera o valor do parametro init de uma simulação
  (define (altera_init elem)
    (if (and (integer? elem)(>= elem 0))
        (begin (vector-set! por_defeito 1 elem)
               (vector-set! v_lidos 1 (+ (vector-ref v_lidos 1) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  ;altera o valor do parametro steps de uma simulação
  (define (altera_steps elem)
    (if (and (integer? elem)(>= elem 1))
        (begin (vector-set! por_defeito 2 elem)
               (vector-set! v_lidos 2 (+ (vector-ref v_lidos 2) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  ;altera o valor do parametro trace de uma simulação
  (define (altera_trace elem)
    (if (or (equal? elem "on")(equal? elem "off"))
        (begin (vector-set! por_defeito 3 elem)
               (vector-set! v_lidos 3 (+ (vector-ref v_lidos 3) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2)))) 
  
  ;altera o valor do parametro albedo de uma simulação
  (define (altera_albedo elem)
    (if (and (rational? elem)(>= elem 0 )(<= elem 1))
        (begin (vector-set! por_defeito 4 elem)
               (vector-set! v_lidos 4 (+ (vector-ref v_lidos 4) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  ;altera o valor do parametro lumino de uma simulação
  (define (altera_lumino elem)
    (if (procedure? (eval elem))
        (begin (vector-set! por_defeito 5 elem)
               (vector-set! v_lidos 5 (+ (vector-ref v_lidos 5) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  ;altera o valor do parametro densidade de uma simulação
  (define (altera_densidade elem)
    (if (and (rational? elem)(>= elem 0)(<= elem 1))
        (begin (vector-set! por_defeito 6 elem)
               (vector-set! v_lidos 6 (+ (vector-ref v_lidos 6) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  ;altera o valor do parametro vizinhanca de uma simulação
  (define (altera_vizinhanca elem)
    (if (and (integer? elem)(>= elem 0))
        (begin (vector-set! por_defeito 7 elem)
               (vector-set! v_lidos 7 (+ (vector-ref v_lidos 7) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  ;altera o valor do parametro mutate de uma simulação
  (define (altera_mutate elem)
    (if (and (rational? elem)(>= elem 0 )(<= elem 1))
        (begin (vector-set! por_defeito 8 elem)
               (vector-set! v_lidos 8 (+ (vector-ref v_lidos 8) 1))) 
        (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2))))
  
  (define (argumentos tam i args)
    (if (< i tam)  ;Lê os parâmetros de uma simulação
        (begin (cond ((equal? (list-ref args i) "-seed") (altera_seed (list-ref args (+ i 1))))               
                     ((equal? (list-ref args i) "-init") (altera_init (list-ref args (+ i 1))))
                     ((equal? (list-ref args i) "-steps") (altera_steps (list-ref args (+ i 1))))
                     ((equal? (list-ref args i) "-trace") (altera_trace (list-ref args (+ i 1))))
                     ((equal? (list-ref args i) "-albedo") (altera_albedo (list-ref args (+ i 1))))
                     ((equal? (list-ref args i) "-lumino") (altera_lumino (list-ref args (+ i 1))))
                     ((equal? (list-ref args i) "-densidade") (altera_densidade (list-ref args (+ i 1))))
                     ((equal? (list-ref args i) "-vizinhanca") (altera_vizinhanca (list-ref args (+ i 1)))) 
                     ((equal? (list-ref args i) "-mutate") (altera_mutate (list-ref args (+ i 1)))) 
                     (else (vector-set! v_lidos 0 (+ (vector-ref v_lidos 0) 2)))) 
               (if (<= (vector-ref v_lidos 0) 1) ;se apanhar algum parametro nao válido, acaba logo e não vai verificar os outros paramet.
                   (argumentos tam (+ i 2) args)))))
  
  ;verifica se todos os parametros passados foram válidos (basta consultar o vector v_lidos, se cada elemento for 0 ou 1, entao é uma
  ;simulação válida
  (define (argumentos_correctos_lidos? tam i)
    (if (< i tam)
        (if (<= (vector-ref v_lidos i) 1)
            (argumentos_correctos_lidos? tam (+ i 1))
            #f)
        #t))
  
  (if (and (list? args) (>= (length args) 0) (<= (length args) 18) (even? (length args)))
      (begin (argumentos (length args) 0 args) ;altera o vector com os parametros de uma simulação
             (if (argumentos_correctos_lidos? (vector-length v_lidos) 0) ;validacao de cada parametro
                 (let ((planeta_intern (extern_to_intern planeta (vector-ref por_defeito 4))))
                   (if (planeta? planeta_intern)
                       (vector planeta_intern por_defeito)
                       erro-de-utilizacao))
                 erro-de-utilizacao))  ;erro de utilização
      erro-de-utilizacao))




;Selectores
;simulacao-planeta : simulacao → planeta ∪ String
(define (simulacao-planeta simulacao)
  (if (simulacao? simulacao)
      (vector-ref simulacao 0)
      (erro-de-utilizacao)))

;simulacao-seed : simulacao → inteiro[0,2E31-1] ∪ String
(define (simulacao-seed simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 0)
      (erro-de-utilizacao)))

;simulacao-init : simulacao → inteiro>=0 ∪ String
(define (simulacao-init simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 1)
      (erro-de-utilizacao)))

;simulacao-steps : simulacao → inteiro>=1 ∪ String
(define (simulacao-steps simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 2)
      (erro-de-utilizacao)))

;simulacao-trace : simulacao → {"on",off} ∪ String
(define (simulacao-trace simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 3)
      (erro-de-utilizacao)))

;simulacao-albedo : simulacao → racional[0,1] ∪ String
(define (simulacao-albedo simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 4)
      (erro-de-utilizacao)))

;simulacao-lumino : simulacao → procedimento ∪ String
(define (simulacao-lumino simulacao)
  (if (simulacao? simulacao)
      (eval (vector-ref (vector-ref simulacao 1) 5))
      (erro-de-utilizacao)))

;simulacao-densidade : simulacao → racional[0,1] ∪ String
(define (simulacao-densidade simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 6)
      (erro-de-utilizacao)))

;simulacao-vizinhanca : simulacao → inteiro>=0 ∪ String
(define (simulacao-vizinhanca simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 7)
      (erro-de-utilizacao)))

;simulacao-mutate : simulacao → racional[0,1] ∪ String
(define (simulacao-mutate simulacao)
  (if (simulacao? simulacao)
      (vector-ref (vector-ref simulacao 1) 8)
      (erro-de-utilizacao)))


;Reconhecedores
;simulacao? : universal -> booleano
(define (simulacao? arg)
  
  (define (elementos-validos? opcoes)
    (and (vector? opcoes) ;verifica representação interna da parte dos parametros de uma simulação
         (= (vector-length opcoes) 9)
         (let ((seed (vector-ref opcoes 0))
               (init (vector-ref opcoes 1))
               (steps (vector-ref opcoes 2))
               (trace (vector-ref opcoes 3))
               (albedo (vector-ref opcoes 4))
               (lumino (vector-ref opcoes 5))
               (densidade (vector-ref opcoes 6))
               (vizinhanca (vector-ref opcoes 7))
               (mutate (vector-ref opcoes 8))) ;validação de todos os parametros (e respectivos valores) de uma simulação
           (and (and (integer? seed) (>= seed 0) (<= seed (- (expt 2 31) 1)))
                (and (integer? init) (>= init 0))
                (and (integer? steps) (>= steps 1))
                (and (string? trace)
                     (or (equal? trace "on") (equal? trace "off")))
                (and (rational? albedo) (>= albedo 0 ) (<= albedo 1))
                (if (string? lumino)
                    (equal? lumino "lumino")
                    (procedure? (eval lumino)))
                (and (rational? densidade) (>= densidade 0) (<= densidade 1))
                (and (integer? vizinhanca) (>= vizinhanca 0))
                (and (rational? mutate)(>= mutate 0 )(<= mutate 1))))))
  
  (and (vector? arg) 
       (= (vector-length arg) 2) ;uma simulação tem que ser um vector com dois elementos
       (and (planeta? (vector-ref arg 0))
            (elementos-validos? (vector-ref arg 1)))))