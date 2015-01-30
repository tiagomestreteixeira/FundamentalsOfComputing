(require (lib "include.ss"))
(include "proj3-posicao-fp0708-32.scm") 
(include "proj3-margarida-fp0708-32.scm")
(include "proj3-aux-fp0708-32.scm")
(require "tokenize.zo")
(require "interface.zo")


; Tipo Abstracto de Informação: Planeta
; Representação interna utilizando vectores com 2 posições em que o primeiro elemento é o albedo da 
; terra inabitada de um planeta e o segundo é outro vector com cada linha de um planeta.
; Cada linha de um planeta também é um vector em que o nº de elementos de cada linha é o nº de colunas e cada elemento é uma posição de
; um planeta. Uma posição ou tém ou margarida ou um vector vazio (posição vazia).

; novo_planeta : Natural × Natural × [0, 1] → Planeta ∪ String
(define (novo_planeta linhas colunas alb)
  
  ;Constroi uma linha em que o nº de elementos é o nº de colunas
  (define (faz-uma-linha n-colunas linha-actual) 
    (if (< n-colunas 1)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (faz-uma-linha (- n-colunas 1) (vector-append (vector (make-vector 0)) linha-actual)))) ;cria uma linha com posições vazias
  
  ;Junta linha a linha afim de totalizar o nº de linhas num planeta
  (define (junta-linhas n-linhas planeta-actual uma-linha)
    (if (= n-linhas 0)
        planeta-actual ;resultado da junção encontra-se em lista-actual
        (junta-linhas (- n-linhas 1) (vector-append planeta-actual uma-linha) uma-linha))) ;junta + 1 uma linha
  
  (if (and (rational? alb) (>= alb 0) (<= alb 1);validação dos argumentos
           (natural? linhas)
           (natural? colunas))
      (vector alb (if (or (= linhas 0) (= colunas 0)) ;se o nº colunas ou o nº linhas for 0 entao o mundo não tém posições
                      (make-vector 0)
                      (junta-linhas linhas (make-vector 0) (vector (faz-uma-linha colunas (make-vector 0)))))) ; senão vai construir as
      ; posições
      erro-input-novo-planeta));erro de input


;albedo_planeta : Planeta -> [0,1] U String
(define (albedo_planeta planeta)
  (if (planeta? planeta) ;se for um planeta, é apenas selecionar o primeiro elemento do mesmo
      (vector-ref planeta 0)
      (if (erro-de-continuacao? planeta) 
          planeta
          erro-input-albedo-planeta))) ;erro de input 

; linhas_planeta : Planeta → Natural ∪ String
(define (linhas_planeta planeta)
  (if (planeta? planeta)           ;se for um planeta, é apenas ver a dimensão do segundo elemento da representação interna
      (vector-length (vector-ref planeta 1))
      (if (erro-de-continuacao? planeta)
          planeta
          erro-input-linhas-planeta))); erro de input

; colunas_planeta : Planeta → Natural ∪ String
(define (colunas_planeta planeta)  
  (if (planeta? planeta)  ;é um planeta ?
      (if (not (vector-empty? (vector-ref planeta 1))) ; se for um planeta sem posições entao o nº de colunas eh 0
          (vector-length (vector-ref (vector-ref planeta 1) 0)) ; senão é o comprimento de uma linha
          0)
      (if (erro-de-continuacao? planeta)
          planeta
          erro-input-colunas-planeta))) ;erro de input

; nasce_margarida : Planeta × Posicao × Margarida → Planeta ∪ String
(define (nasce_margarida planeta posicao margarida)
  
  ;Vai alterar uma linha do mundo na qual nasce a margarida
  (define (altera-linha linha-planeta n-colunas linha-actual n-actual) 
    (if (= n-actual n-colunas)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (altera-linha linha-planeta 
                      n-colunas
                      (vector-append linha-actual ;junta elemento a elemento da linha
                                     (if (= (coluna_posicao posicao) n-actual)
                                         (vector margarida)  ;se for esta a posição, entao adiciona-se a margarida dada como argumento
                                         (vector (vector-ref linha-planeta n-actual)))) 
                      (+ n-actual 1))))
  
  
  ;Junta linha a linha afim de construir o novo planeta
  (define (faz-nascer-margarida tabuleiro n-linhas planeta-actual linha-actual)
    (if (= n-linhas linha-actual)
        planeta-actual ;resultado da junção encontra-se em planeta-actual
        (faz-nascer-margarida tabuleiro 
                              n-linhas 
                              (vector-append planeta-actual ;junta a linha actual
                                             (if (= (linha_posicao posicao) linha-actual) ; É nesta linha que vai nascer?
                                                 (vector (altera-linha (vector-ref tabuleiro linha-actual) ;constroi linha com a margarida
                                                                       (colunas_planeta planeta) 
                                                                       (make-vector 0) 
                                                                       0)) 
                                                 (vector (vector-ref tabuleiro linha-actual)))) ;se não for, entao 
                              (+ linha-actual 1))))                                              ;a linha permanece inalterada
                              
  
  (if (and (planeta? planeta) (posicao? posicao) (margarida? margarida)) ;validação dos argumentos
      (if (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;é posição válida?
          (if (margarida? (vector-ref (vector-ref (vector-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao)))
              planeta ;se na posição se encontrar uma margarida eh so devolver o planeta inalterado
              (vector (albedo_planeta planeta) (faz-nascer-margarida (vector-ref planeta 1) 
                                                                     (linhas_planeta planeta) 
                                                                     (make-vector 0) 
                                                                     0)));novo planeta
          erro-consistencia-nasce-margarida) ;erro de consistencia
      (cond ((erro-de-continuacao? planeta) planeta)
            ((erro-de-continuacao? posicao) posicao)
            ((erro-de-continuacao? margarida) margarida)
            (else
             erro-input-nasce-margarida)))) ;erro de input


; morre_margarida : Planeta × Posicao → Planeta ∪ String
(define (morre_margarida planeta posicao)
  
  ;Permite alterar uma linha de um planeta
  (define (altera-linha linha-planeta n-colunas linha-actual n-actual) 
    (if (= n-actual n-colunas)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (altera-linha linha-planeta 
                      n-colunas
                      (vector-append linha-actual
                                     (if (= (coluna_posicao posicao) n-actual)  ;se a margarida a matar estiver nesta posição, então
                                         (vector (make-vector 0)) ; esta posição será uma posição vazia
                                         (vector (vector-ref linha-planeta n-actual)))) ;senao é o que lá estiver dentro
                      (+ n-actual 1))))
  
  ;Junta linha a linha afim de totalizar o nº de linhas num mundo, percorre-se linha a linha
  (define (mata-margarida tabuleiro n-linhas planeta-actual linha-actual)
    (if (= n-linhas linha-actual)
        planeta-actual ;resultado da junção encontra-se em planeta-actual
        (mata-margarida tabuleiro 
                        n-linhas 
                        (vector-append planeta-actual
                                       (if (= (linha_posicao posicao) linha-actual) ;se a posição for nesta linha, entao altera-se a linha
                                           (vector (altera-linha (vector-ref tabuleiro linha-actual) (colunas_planeta planeta) 
                                                                 (make-vector 0) 0))
                                           (vector (vector-ref tabuleiro linha-actual)))) ;senão a mesma permanece inalterada
                        (+ linha-actual 1))))
  
  (if (and (planeta? planeta) (posicao? posicao)) ;validação dos inputs
      (if (and (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;é posicao válido e está lá uma margarida?
               (margarida? (vector-ref (vector-ref (vector-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao))))
          (vector (albedo_planeta planeta) (mata-margarida (vector-ref planeta 1) (linhas_planeta planeta) (make-vector 0) 0))
          erro-consistencia-morre-margarida) ;senao se verificam as duas : erro de consistencia
      (cond ((erro-de-continuacao? planeta) planeta)
            ((erro-de-continuacao? posicao) posicao)
            (else
             erro-input-morre-margarida))))
       ;erro de input



; obtem_margarida : Planeta × Posicao → Margarida ∪ String
(define (obtem_margarida planeta posicao)
  (if (and (planeta? planeta) (posicao? posicao)) ;validação dos inputs
      (if (and (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;se for uma posicao válida e se a mesma
               (margarida? (vector-ref (vector-ref (vector-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao)))); contiver
                                                                                                                              ; uma
          (vector-ref (vector-ref (vector-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao));margarida entao seleciona 
          erro-consistencia-obtem-margarida) ;senao dá erro de consistência                                        a margarida
       (cond ((erro-de-continuacao? planeta) planeta)
            ((erro-de-continuacao? posicao) posicao)
            (else
             erro-input-obtem-margarida)))) ;erro de input

; tem_margarida? : Planeta × Posicao → Lógico ∪ String
(define (tem_margarida? planeta posicao)
  (if (and (planeta? planeta) (posicao? posicao)) ;validação dos inputs 
      (if (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)) ;a posição é válida?,se sim ve se existe na
          (margarida? (vector-ref (vector-ref (vector-ref planeta 1) (linha_posicao posicao)) (coluna_posicao posicao))) ;dada posicao
          erro-consistencia-tem-margarida?) ;se nao for posicao valida ==> erro de consistencia
      (cond ((erro-de-continuacao? planeta) planeta)
            ((erro-de-continuacao? posicao) posicao)
            (else
             erro-input-tem-margarida?)))) ;erro de input

; planeta? : Universal → Lógico
; Este procedimento determina se determinado argumento se enquadra na representação interna de um planeta
(define (planeta? arg) ; ou seja, se é de facto um planeta
  
  ;Verifica se todos os elementos de cada posição do planeta são elementos de um planeta
  (define (elementos-validos? tabuleiro index size)
    (if (= index size) ;se chegarmos ao fim do planeta, entao todas as condições são válidas
        #t
        (if (todos-satisfazem? (vector-ref tabuleiro index) (lambda (x) (or (margarida? x) (vector-empty? x))) 0 (vector-length 
                                                                                                                  (vector-ref tabuleiro
                                                                                                                                index)))
            (elementos-validos? tabuleiro (+ index 1) size) ;vai continuar a verificar os restantes já que o actual eh válido 
            #f)));Se nesta fase da recursao houver um elemento que não for válido, então paramos e é #f
  
  (if (and (vector? arg) (= (vector-length arg) 2)) ;Tem que ser um vector com 2 elementos
      (let ((val_alb (vector-ref arg 0)) ;val_alb = valor do albedo
            (tabuleiro (vector-ref arg 1))) ; tabuleiro = vector com as posicoes do mundo (em forma de "matriz") 
        (if (and (rational? val_alb) ; albedo racional entre [0,1]
                 (>= val_alb 0)
                 (<= val_alb 1))
            (if (vector-empty? tabuleiro) ;se o tabuleiro for vector vazio, entao o mundo não tem posições (ou o nº de linhas eh 0
                #t           ;                                                                           ou o nº de colunas eh 0)
                (and (todos-satisfazem? tabuleiro (lambda(x) (and (vector? x) (not (vector-empty? x)))) 0 (vector-length tabuleiro)) 
                     ;verifica se todas as linhas sao vectores não
                     (let ((size (vector-length (vector-ref tabuleiro 0)))) 
                       ;size = nº de colunas de uma linha                                
                       (todos-satisfazem? tabuleiro (lambda(x) (= (vector-length x) size)) 0 (vector-length tabuleiro))) 
                     ;todas as linhas têm que ter o mesmo size 
                     (elementos-validos? tabuleiro 0 (vector-length tabuleiro)))) ; verifica se cada posição contém 
                                                                                  ; um elemento de um planeta
            #f))
      #f))

; extern_to_intern : String × [0, 1] → Planeta ∪ String
(define (extern_to_intern rep-externa alb)
  
  ;Constroi uma linha em que o nº de elementos é o nº de colunas
  (define (produz-linha string-linha n-colunas linha-actual n-actual) 
    (if (= n-actual n-colunas)
        linha-actual ;a linha vai ser armazenada no nome linha-actal
        (produz-linha string-linha 
                      n-colunas
                      (vector-append linha-actual
                                     (if (= (string-length (list-ref string-linha n-actual)) 1)
                                         (if (string=? (list-ref string-linha n-actual) " ") ;se for um espaço
                                             (vector (make-vector 0)) ;posição vazia
                                             (vector "Character-invalido")) ;Garante verificação do argumento inicial      
                                         (if (= 3 (length (tokenize (list-ref string-linha n-actual) #\,)))
                                             (let* ((marga (tokenize (list-ref string-linha n-actual) #\,))                                                      (alb (string->number (list-ref marga 0)));margarida e partimos a string em 3
                                                    (t-min (string->number (list-ref marga 1)));            (separando por ','))
                                                    (t-max (string->number (list-ref marga 2))))
                                               (vector (nova_margarida alb t-min t-max)))
                                             (vector "Character-invalido"))));Garante que a string da rep. externa não tem qualquer tipo de
                      (+ n-actual 1))))                                     ;combinações de caracteres não detectados anteriormente
  
  
  ;Junta linha a linha afim de totalizar o nº de linhas num mundo
  (define (junta-linhas n-linhas vector-actual linha-actual linhas)
    (if (= n-linhas linha-actual)
        vector-actual ;resultado da junção encontra-se em vector-actual
        (junta-linhas n-linhas 
                      (vector-append vector-actual  ;partimos por | (dá uma lista com os elementos
                                     (vector (produz-linha (tokenize (list-ref linhas linha-actual) #\|) ; de uma linha
                                                           (length (tokenize (list-ref linhas linha-actual) #\|)) (make-vector 0) 0))) 
                      (+ linha-actual 1) linhas)))
  
  
  (if (and (string? rep-externa) ;validação (inicial) dos argumentos 
           (rational? alb)
           (>= alb 0)
           (<= alb 1))
      (if (erro-de-continuacao? rep-externa)
          rep-externa
          (if (= (string-length rep-externa) 0) ;o mundo neste caso é vazio
              (vector alb (make-vector 0)) ;devolve mundo vazio                    
              (let ((planeta 
                     (if (existe-char? #\/ rep-externa) ;a string passada tem que ter pelo menos uma ocorrencia do caracter /
                         (vector alb 
                                 (junta-linhas 
                                  (- (length (tokenize rep-externa #\/)) 1) 
                                  (make-vector 0) 
                                  0 
                                  (tokenize rep-externa #\/)))
                         "String nao valida/"))) ;(em baixo partimos as linhas (tokens separados por /)
                (if (planeta? planeta) ;no final ficou a rep. externa de um planeta?
                    planeta ;ok ficou
                    erro-input-extern-to-intern)))) ; not ok, nao ficou -> erro de input
      erro-input-extern-to-intern))

; intern_to_extern : Planeta → String
(define (intern_to_extern planeta)
  ;transforma uma linha da representação interna numa das strings pela qual é composta a string final
  (define (string-linha linha index size)
    (if (= size index)  ;se chegar ao fim da linha: dá caracter de final de linha : /
        "/"
        (string-append (let((elemento (vector-ref linha index))) ;senao, temos que ver se o elemento actual é uma margarida, e se for
                         (if (margarida? elemento)         ; temos o transformador de saida da mesma e eh so adicionar á string
                             (imprime_margarida elemento)
                             " ")) ;se nao for uma margarida é uma posição vazia
                       (if (not (= size (+ index 1))) 
                           "|" ;caracter de separação
                           "")
                       (string-linha linha (+ index 1) size))))
  
  ;Junta todas as linhas produzidas através de cada linha da representação interna de forma a construir um planeta
  (define (percorre tabuleiro string-actual index size)
    (if (= index size)
        string-actual                                                   ;transforma uma linha numa string que vai ser juntada á final
        (percorre tabuleiro 
                  (string-append string-actual 
                                 (string-linha (vector-ref tabuleiro index) 
                                               0 
                                               (vector-length (vector-ref tabuleiro index))))   
                  (+ index 1) 
                  size)))
  
  (if (planeta? planeta) ;Validação do argumento
      (percorre (vector-ref planeta 1) "" 0 (vector-length (vector-ref planeta 1))) ;Se o planeta recebido for vazio, não existem posições, nem linhas, nem colunas, logo o
      (if (erro-de-continuacao? planeta)
          planeta
          erro-input-intern-to-extern)));                                                                                  resultado é uma string vazia
;                                                                             PS: DE NOTAR QUE SE O MUNDO FOR VAZIO, 
;                                                                                 O MOSTRA_MUNDO DÁ LOGO MAPA VAZIO !!!!!!



;copia_planeta : Planeta → Planeta ∪ String
(define (copia_planeta planeta)
  
  ;Junta linha a linha afim de construir o novo planeta
  (define (constroi tabuleiro n-linhas planeta-actual linha-actual)
    (if (= n-linhas linha-actual)
        planeta-actual ;resultado da junção encontra-se em planeta-actual
        (constroi tabuleiro 
                  n-linhas 
                  (vector-append planeta-actual ;junta a linha actual
                                 (vector (vector-ref tabuleiro linha-actual)))
                  (+ linha-actual 1))))
  
  (if (planeta? planeta) ;validação dos argumentos
      (vector (albedo_planeta planeta) 
              (constroi (vector-ref planeta 1) 
                        (linhas_planeta planeta) 
                        (make-vector 0) 
                        0));novo planeta
      (if (erro-de-continuacao? planeta)
           planeta
           erro-input-copia-planeta))) ;erro de input



  