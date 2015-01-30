(require (lib "include.ss"))
(include "proj2-posicao-fp0708-32.scm") 
(include "proj2-margarida-fp0708-32.scm")
(include "proj2-aux-fp0708-32.scm")
(require "tokenize.zo")
(require "interface.zo")


; Tipo Abstracto de Informação: Planeta
; Representação interna utilizando listas primitivas do scheme com 2 posições em que o primeiro elemento é o albedo da 
; terra inabitada de um planeta e o segundo é outra lista com a representação de cada posição. Cada posição desta lista é uma linha.
; Cada linha também é uma lista em que o nº de elementos de cada uma é o nº de colunas. Uma posição ou tém ou margarida ou uma lista
; vazia.


; novo_planeta : Natural × Natural × [0, 1] → Planeta ∪ String
(define (novo_planeta linhas colunas alb)
  
  ;Constroi uma linha em que o nº de elementos é o nº de colunas
  (define (faz-uma-linha n-colunas linha-actual) 
    (if (< n-colunas 1)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (faz-uma-linha (- n-colunas 1) (append (list null) linha-actual)))) ;cria uma linha com posições vazias
  
  ;Junta linha a linha afim de totalizar o nº de linhas num mundo
  (define (junta-linhas n-linhas lista-actual uma-linha)
    (if (= n-linhas 0)
        lista-actual ;resultado da junção encontra-se em lista-actual
        (junta-linhas (- n-linhas 1) (append lista-actual uma-linha) uma-linha))) ;junta + 1 uma linha
  
  (if (and (rational? alb) (>= alb 0) (<= alb 1);validação dos argumentos
           (natural? linhas)
           (natural? colunas))
      (list alb (if (or (= linhas 0) (= colunas 0)) ;se o nº colunas ou o nº linhas for 0 entao o mundo não tém posições
                    null
                    (junta-linhas linhas null (list (faz-uma-linha colunas null))))) ; senão vai construir as posições
      erro-de-input)) ;erro de input


;albedo_planeta : Planeta -> [0,1] U String
(define (albedo_planeta planeta)
  (if (planeta? planeta) ;se for um planeta, é apenas selecionar o primeiro elemento do mesmo
      (list-ref planeta 0)
      erro-de-input)) ;erro de input 

; linhas_planeta : Planeta → Natural ∪ String
(define (linhas_planeta planeta)
  (if (planeta? planeta)           ;se for um planeta, é apenas ver a dimensão do segundo elemento da representação interna
      (length (list-ref planeta 1))
      erro-de-input)) ; erro de input

; colunas_planeta : Planeta → Natural ∪ String
(define (colunas_planeta planeta)  
  (if (planeta? planeta)  ;é um planeta ?
      (if (not (= (length (list-ref planeta 1)) 0)) ; se for um planeta sem posições entao o nº de colunas eh 0
          (length (list-ref (list-ref planeta 1) 0)) ; senão é o comprimento de uma linha
          0)
      erro-de-input)) ;erro de input

; nasce_margarida : Planeta × Posicao × Margarida → Planeta ∪ String
(define (nasce_margarida planeta posicao margarida)
  
  ;Vai alterar uma linha do mundo na qual nasce a margarida
  (define (altera-linha linha-planeta n-colunas linha-actual n-actual) 
    (if (= n-actual n-colunas)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (altera-linha linha-planeta 
                      n-colunas
                      (append linha-actual ;junta elemento a elemento da linha
                              (if (= (coluna_posicao posicao) n-actual)
                                  (list margarida)  ;se for esta a posição, entao adiciona-se a margarida dada como argumento
                                  (list (list-ref linha-planeta n-actual)))) 
                      (+ n-actual 1))))
  
  
  ;Junta linha a linha afim de construir o novo planeta
  (define (faz-nascer-margarida tabuleiro n-linhas lista-actual linha-actual)
    (if (= n-linhas linha-actual)
        lista-actual ;resultado da junção encontra-se em lista-actual
        (faz-nascer-margarida tabuleiro 
                        n-linhas 
                        (append lista-actual ;junta a linha actual
                                (if (= (linha_posicao posicao) linha-actual) ; se esta for a linha em que temos que fazer nascer,então é
                                    (list (altera-linha (list-ref tabuleiro linha-actual) ;fazer uma nova linha
                                                        (colunas_planeta planeta) 
                                                        null 
                                                        0)) 
                                    (list (list-ref tabuleiro linha-actual)))) ;se não for, entao a linha permanece inalterada
                        (+ linha-actual 1))))
  
  (if (and (planeta? planeta) (posicao? posicao) (margarida? margarida)) ;validação dos argumentos
      (if (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;é posição válida?
          (if (margarida? (list-ref (list-ref (list-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao)))
              planeta ;se na posição se encontrar uma margarida eh so devolver o planeta inalterado
              (list (albedo_planeta planeta) (faz-nascer-margarida (list-ref planeta 1) (linhas_planeta planeta) null 0)));novo planeta
          erro-de-consistencia) ;erro de consistencia
      erro-de-input)) ;erro de input


; morre_margarida : Planeta × Posicao → Planeta ∪ String
(define (morre_margarida planeta posicao)
  
  ;Permite alterar uma linha de um planeta
  (define (altera-linha linha-planeta n-colunas linha-actual n-actual) 
    (if (= n-actual n-colunas)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (altera-linha linha-planeta 
                      n-colunas
                      (append linha-actual
                              (if (= (coluna_posicao posicao) n-actual)  ;se a margarida a matar estiver nesta posição, então
                                  (list null) ; esta posição será uma posição vazia
                                  (list (list-ref linha-planeta n-actual)))) ;senao é o que lá estiver dentro
                      (+ n-actual 1))))
  
  ;Junta linha a linha afim de totalizar o nº de linhas num mundo, percorre-se linha a linha
  (define (mata-margarida tabuleiro n-linhas lista-actual linha-actual)
    (if (= n-linhas linha-actual)
        lista-actual ;resultado da junção encontra-se em lista-actual
        (mata-margarida tabuleiro 
                        n-linhas 
                        (append lista-actual
                                (if (= (linha_posicao posicao) linha-actual) ;se a posição for nesta linha, entao altera-se a linha
                                    (list (altera-linha (list-ref tabuleiro linha-actual) (colunas_planeta planeta) null 0))
                                    (list (list-ref tabuleiro linha-actual)))) ;senão a mesma permanece inalterada
                        (+ linha-actual 1))))
  
  (if (and (planeta? planeta) (posicao? posicao)) ;validação dos inputs
      (if (and (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;é posicao válido e está lá uma margarida?
               (margarida? (list-ref (list-ref (list-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao))))
          (list (albedo_planeta planeta) (mata-margarida (list-ref planeta 1) (linhas_planeta planeta) null 0))
          erro-de-consistencia) ;senao se verificam as duas : erro de consistencia
      erro-de-input)) ;erro de input



; obtem_margarida : Planeta × Posicao → Margarida ∪ String
(define (obtem_margarida planeta posicao)
  (if (and (planeta? planeta) (posicao? posicao)) ;validação dos inputs
      (if (and (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;se for uma posicao válida e se a mesma
               (margarida? (list-ref (list-ref (list-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao))));contiver uma
          (list-ref (list-ref (list-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao));margarida entao seleciona 
          erro-de-consistencia) ;senao dá erro de consistência                                        a margarida
      erro-de-input)) ;erro de input

; tem_margarida? : Planeta × Posicao → Lógico ∪ String
(define (tem_margarida? planeta posicao)
  (if (and (planeta? planeta) (posicao? posicao)) ;validação dos inputs 
      (if (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;a posição é válida?,se sim ve se existe na
          (margarida? (list-ref (list-ref (list-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao))) ;dada posicao
          erro-de-consistencia) ;se nao for posicao valida ==> erro de consistencia
      erro-de-input)) ;erro de input

; planeta? : Universal → Lógico
; Este procedimento determina se determinado argumento se enquadra na representação interna de um planeta
(define (planeta? arg) ; ou seja, se é de facto um planeta
  
  ;Procedimento de ordem superior que devolve #t se todos os elementos de uma lista satisfazem
  (define (todos-satisfazem? lst pred) ; o predicado passado no segundo argumento
    (if (null? lst)
        #t
        (if (pred (list-ref lst 0)) ;O elemento actual satisfaz o predicado?
            (todos-satisfazem? (list-tail lst 1) pred) ;se sim entao vai ver os elementos seguintes
            #f)))
  
  ;Verifica se todos os elementos de cada posição do planeta são elementos de um planeta
  (define (elementos-validos? tabuleiro)
    (if (null? tabuleiro) ;se a chegarmos ao fim do planeta, entao todas as condições são válidas
        #t
        (if (todos-satisfazem? (list-ref tabuleiro 0) (lambda (x) (or (margarida? x) (null? x))))
            (elementos-validos? (list-tail tabuleiro 1)) ;vai continuar a verificar os restantes já que o actual eh válido 
            #f))) ;se nesta fase da recurso houver um elemento que não for válido, então paramos e é #f
  
  (if (and (list? arg) (= (length arg) 2)) ;Tem que ser uma lista com 2 elementos
      (let ((val_alb (list-ref arg 0)) ;val_alb = valor do albedo
            (tabuleiro (list-ref arg 1))) ; tabuleiro = lista com as posicoes do mundo (em forma de "matriz") 
        (if (and (rational? val_alb) ; albedo racional entre [0,1]
                 (>= val_alb 0)
                 (<= val_alb 1)
                 (null? tabuleiro)) ;se o tabuleiro for lista vazia, entao o mundo não tem posições (ou o nº de linhas eh 0
            #t           ;                                                                           ou o nº de colunas eh 0)
            (and (todos-satisfazem? tabuleiro (lambda(x) (and (list? x) (not (null? x))))) ;verifica se todas as linhas sao listas não
                 (let ((size (length (list-ref tabuleiro 0)))) ;size = nº de colunas de uma linha                                ;nulas
                   (todos-satisfazem? tabuleiro (lambda(x) (= (length x) size)))) ;todas as linhas têm que ter o mesmo size 
                 (elementos-validos? tabuleiro)))) ; verifica se cada posição contém um elemento de um planeta
      #f))

; extern_to_intern : String × [0, 1] → Planeta ∪ String
(define (extern_to_intern rep-externa alb)
  
  ;Constroi uma linha em que o nº de elementos é o nº de colunas
  (define (produz-linha string-linha n-colunas linha-actual n-actual) 
    (if (= n-actual n-colunas)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (produz-linha string-linha 
                      n-colunas
                      (append linha-actual
                              (if (= (string-length (list-ref string-linha n-actual)) 1) ;se for um espaço vazio comp=1
                                  (list null) ;posição vazia
                                  (let* ((marga (tokenize (list-ref string-linha n-actual) #\,)) ;senao significa que lá está uma 
                                         (alb (string->number (list-ref marga 0)));               margarida e partimos a string em 3
                                         (t-min (string->number (list-ref marga 1)));            (separando por ','))
                                         (t-max (string->number (list-ref marga 2))))
                                    (list (nova_margarida alb t-min t-max))))) ;nova margarida para pos actual
                      (+ n-actual 1)))) 
 
  
  ;Junta linha a linha afim de totalizar o nº de linhas num mundo
  (define (junta-linhas n-linhas lista-actual linha-actual linhas)
    (if (= n-linhas linha-actual)
        lista-actual ;resultado da junção encontra-se em lista-actual
        (junta-linhas n-linhas (append lista-actual                                   ;partimos por | (dá uma lista com os elementos
                                       (list (produz-linha (tokenize (list-ref linhas linha-actual) #\|) ; de uma linha
                                                           (length (tokenize (list-ref linhas linha-actual) #\|)) null 0))) 
                      (+ linha-actual 1) linhas)))
  
  
  (if (and (string? rep-externa) ;validação dos argumentos
           (rational? alb)
           (>= alb 0)
           (<= alb 1))
      (if (= (string-length rep-externa) 0) ;o mundo neste caso é vazio
          (list alb null) ;devolve mundo vazio                                   (em baixo partimos as linhas (tokens separados por /) 
          (list alb (junta-linhas (- (length (tokenize rep-externa #\/)) 1) null 0 (tokenize rep-externa #\/))))
      erro-de-input))


; intern_to_extern : Planeta → String
(define (intern_to_extern planeta)
  ;transforma uma linha da representação interna numa das strings pela qual é composta a string final
  (define (string-linha linha)
    (if (null? linha)  ;se chegar ao fim da linha: dá caracter de final de linha : /
        "/"
        (string-append (let((elemento (list-ref linha 0))) ;senao, temos que ver se o elemento actual é uma margarida, e se for
                         (if (margarida? elemento)         ; temos o transformador de saida da mesma e eh so adicionar á string
                             (imprime_margarida elemento)
                             " ")) ;se nao for uma margarida é uma posição vazia
                       (if (not (null? (list-tail linha 1))) 
                           "|" ;caracter de separação
                           "")
                       (string-linha (list-tail linha 1)))))
  
  ;Junta todas as linhas produzidas através de cada linha da representação interna de forma a construir um planeta
  (define (percorre tabuleiro string-actual)
    (if (null? tabuleiro)
        string-actual                                                   ;transforma uma linha numa string que vai ser juntada á final
        (percorre (list-tail tabuleiro 1) (string-append string-actual (string-linha (list-ref tabuleiro 0))))))
  
  (if (planeta? planeta) ;Validação do argumento
      (percorre (list-ref planeta 1) "") ;Se o planeta recebido for vazio, não existem posições, nem linhas, nem colunas, logo o
      erro-de-input));                                                                                  resultado é uma string vazia
;                                                                             