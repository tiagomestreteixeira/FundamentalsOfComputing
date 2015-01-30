(require (lib "include.ss"))
(include "proj3-margarida-fp0708-32.scm")
(include "proj3-planeta-fp0708-32.scm")
(include "proj3-aux-fp0708-32.scm")
(include "proj3-posicao-fp0708-32.scm")
(include "proj3-simulador-fp0708-32.scm")
(require "gerador.zo")
(require "interface.zo")

;--------------------------------;
;       1ª Fase DaisyWorld       ;
;--------------------------------;

;1) Definir as Constantes

;Constante de Stefan Boltzmann
(define stefan-boltzmann (* 5.67 (expt 10 -8)))

;Constante Fluxo Solar
(define fluxo-solar 917)

;Constante que contém a emissividade do planeta
(define emissividade-planeta 0.5)

;Constante que contém o factor de absorção
(define factor-absorcao 20)

;2)
;Mostra o valor das constantes definidas em cima
(define (mostra_const)
  (display "Constante de Stefan Boltzmann: ")
  (display stefan-boltzmann)
  (newline)
  (display "Fluxo solar: ")
  (display fluxo-solar)
  (newline)
  (display "Emissividade do planeta: ")
  (display emissividade-planeta)
  (newline)
  (display "Factor de absorção: ")
  (display factor-absorcao))

;3)
;Calcula a luminosidade solar para um determinado instante de tempo
(define lumino
  (lambda(tempo)
    (if (and (>= tempo 0) (integer? tempo)) ;Validação do argumento tempo
        (+ (* (/ 1.2 200) tempo) 
           0.6)
        erro-de-input))) ;Erro de input.

;4)
;Calcula o albedo de uma região
(define (alb_regiao alb-brancas alb-pretas alb-inabitada super-ocu-brancas super-ocu-pretas)
  ;Percentagem da superficie inabitada : 1 - (superficie ocupada pelas brancas + s. ocupada pelas pretas) - Valor em percentagem
  (define super-inabitada (- 1 (+ super-ocu-brancas super-ocu-pretas)))     
  
  (if (and (and (rational? alb-brancas) (>= alb-brancas 0) (<= alb-brancas 1))
           (and (rational? alb-pretas) (>= alb-pretas 0) (<= alb-pretas 1))
           (and (rational? alb-inabitada) (>= alb-inabitada 0) (<= alb-inabitada 1))
           (and (rational? super-ocu-brancas) (>= super-ocu-brancas 0) (<= super-ocu-brancas 1))
           (and (rational? super-ocu-pretas) (>= super-ocu-pretas 0) (<= super-ocu-pretas 1))) ;Validação dos inputs      
      (+ (* alb-brancas super-ocu-brancas)  ;Fórmula do albedo
         (* alb-pretas super-ocu-pretas)
         (* alb-inabitada super-inabitada))
      erro-de-input)) ;Erro de input

;5)
;Calcula a temperatura de um planeta num determinado periodo de tempo e segundo um albedo
(define (T_planeta luminosidade albedo)
  (if (and (and (rational? luminosidade) (>= luminosidade 0)) ;Validação dos dados de input
           (and (rational? albedo) (>= albedo 0) (<= albedo 1)))
      (- (sqrt   ;De notar que a raíz quarta é igual á aplicação da raíz quadrada á raíz quadrada
          (sqrt (/ (* fluxo-solar luminosidade (- 1 albedo))
                   (* emissividade-planeta stefan-boltzmann))))
         273.15) ;Fórmula da temperatura do planeta (com duas raízes quadradas).
      erro-de-input)) ;Erro de input.

;6)
;Calcula o conforto Térmico de uma margarida num determinado instante
(define (conforto tempo alb-pla alb-regiao-margarida alb-margarida T-minima T-maxima)
  
  (if (and (and (integer? tempo) (>= tempo 0))
           (and (rational? alb-pla) (>= alb-pla 0) (<= alb-pla 1))
           (and (rational? alb-regiao-margarida) (>= alb-regiao-margarida 0) (<= alb-regiao-margarida 1))
           (and (rational? alb-margarida) (>= alb-margarida 0) (<= alb-margarida 1))
           (integer? T-minima)
           (integer? T-maxima)) ;Validação dos inputs
      
      (let((formula-conforto (- 1 (* (/ 4 (quadrado (- T-maxima T-minima))) ;é atribuído ao nome criado pelo let (formula-conforto), 
                                     (quadrado (- (Ti T-minima T-maxima)    ;o valor resultante da fórmula do conforto 
                                                  (T-loc (T_planeta (lumino tempo) alb-pla) alb-margarida alb-regiao-margarida)))))))
        (cond ((> formula-conforto 1) 1)  ;Se exceder o valor 1
              ((< formula-conforto -1) -1) ;Se exceder o valor -1
              (else formula-conforto))) ;Senao dá o valor calculado. De notar que o let permitiu calcular o valor uma única vez
      erro-de-input);Erro de input.                                                                                        
  )

;Temperatura Local
(define (T-loc temp-planeta alb-margarida alb-vizinhanca) 
  (+ (* factor-absorcao (- alb-vizinhanca alb-margarida))
     temp-planeta))

;7)
;Determina qual a nova etapa na vida de uma margarida.
(define (etapa conforto n-margaridas-vizinhanca lim-vm)
  (if (and (rational? conforto)  ;Validação do input
           (and (integer? n-margaridas-vizinhanca) (>= n-margaridas-vizinhanca 0))
           (and (integer? lim-vm) (>= lim-vm 0)))
      (if (>= conforto 0)
          (if (< n-margaridas-vizinhanca lim-vm)
              1   ;A Margarida provavelmente irá crescer na sua vizinhança
              2)  ;A Margarida provavelmente irá crescer noutro local
          3)
      erro-de-input)) ;A Margarida provavelmente irá morrer

;8)
;Imprime as coordenadas das células que se encontram na vizinhança de ordem n de uma dada célula
(define (vizinhos_print x y linhas colunas ordem)
  
  ;Vamos imprimir linha a linha, começando na primeira linha e coluna da vizinhança,
  ;se a ordem da vizinhança ultrapassar as dimensões, entao serão impressas as coordenadas definidas até ao limite da dimensão
  
  (define Xinicial ;Coordenada x inicial da vizinhança       
    (if (and (number? x) (number? ordem))
        (if (>= (- x ordem) 0)
            (- x ordem)
            0)
        x))
  
  (define Yinicial  ;Coordenada y inicial da vizinhança
    (if (and (number? y) (number? ordem))
        (if (>= (- y ordem) 0)
            (- y ordem)
            0)
        y))
  
  (define Xfinal    ;Coordenada x final da vizinhança
    (if (and (number? x)(number? linhas) (number? ordem))
        (if (<= (+ x ordem) (- linhas 1))
            (+ x ordem)
            (- linhas 1))
        x))
  
  (define Yfinal    ;Coordenada y final da vizinhança
    (if (and (number? y)(number? colunas) (number? ordem))
        (if (<= (+ y ordem) (- colunas 1))
            (+ y ordem)
            (- colunas 1))
        y))
  
  (define (imprime x y)  ;Vamos percorrer linha a linha, assim sendo a coordenada x actual fica fixa até a coordenada y final ser atingida
    (if (> x Xfinal)     ;então depois, y é reposto á coordenada y inicial e a x é incrementada, até chegarmos á coordenada x final
        (display "") ;Se não houver mais nada para imprimir, nada será impresso
        (imprime-linha x y))) ;senão vamos imprimir
  
  
  (define (imprime-linha x y) ;Imprimimos todas as colunas da linha x
    (if (> y Yfinal)
        (imprime (+ x 1) Yinicial) ;Próxima linha     
        (begin (display "(")
               (display x)
               (display ",")
               (display y)
               (display ")")
               (imprime-linha x (+ y 1))))) ;Próxima coluna
  
  (if (and (and (integer? x) (>= x 0))
           (and (integer? y) (>= y 0))
           (and (integer? linhas) (>= linhas 0))
           (and (integer? colunas) (>= colunas 0))
           (and (integer? ordem) (>= ordem 0))
           (and (< x linhas) (< y colunas))) ;Validação do input
      (imprime Xinicial Yinicial) ;procedimento para imprimir
      erro-de-input)) ;Erro de input


;--------------------------------;
;       2ª Fase DaisyWorld       ;
;--------------------------------;

;1)a devolve #t consoante o input e a formula for correcta, ou #f se um deles falhar
(define (joga_probabilidades p pa)
  (if (and (rational? p)(>= p 0)(<= p 1) ;validação do input
           (integer? pa) (>= pa 0) (<= pa (- (expt 2 31) 2)))
      (< pa (* p  (- (expt 2 31) 1)))
      erro-input-joga-probabilidades)) ;condiçao dada no enunciado

;Temperatura De um planeta para um determinado instante de tempo (Para a segunda fase do projecto)
(define (Tp planeta t)
  (- (sqrt (sqrt (/ (* FS (formula-luminosidade t) (- 1 (alb_regiao planeta))) (* E SB))))
     273.15))

;1)b devolve uma posicao valido do mundo, obtida atraves da formula dada no encunciado
(define (posicao_a_sorte H V pa)
  (define (da_posicao l c H V pos acc)
    (if (< l H) ;verifica se chegou a ultima linha
        (if (< c V) ;verifica se chegou a ultima coluna
            (if (= acc pos) ;chegamos a celula que corresponde ao numero gerado
                (nova_posicao l c) ;é criada uma nova posicao com as coordenadas pertencentes ao tabuleiro
                (da_posicao l (+ c 1) H V pos (+ acc 1))) ;ainda nao se chegou à celula pretendida, deve-se continuar a percorrer
            (da_posicao (+ l 1) 0 H V pos acc)))) ;terminou-se uma linha, deve-se ver a seguinte 
  
  (if (and (integer? H) (>= H 0) (integer? V)(>= V 0) ;validação do input
           (integer? pa) (>= pa 0) (<= pa (- (expt 2 31) 2)))
      (da_posicao 0 0 H V (remainder pa (* H V)) 0) ;obtem o numero gerado atraves da formula dada no enunciado
      erro-input-posicao-a-sorte)) ;o input é incorrecto

;1)c devolve o albedo da nova margarida, podendo este ser o da progenitora ou um albedo gerado devido a uma mutação
(define (novo_albedo albedo_mae prob_M pa)
  (define condicao (/ (if (integer? pa) pa 0) (- (expt 2 31) 2))); ALTERAR ;condicao para decidir qual a forma que será usada para calcular o albedo
  (if (and (rational? albedo_mae) (>= albedo_mae 0) (<= albedo_mae 1) ;verificaçao de input
           (rational? prob_M) (>= prob_M 0) (<= prob_M 1)
           (integer? pa)(>= pa 0) (<= pa (- (expt 2 31) 2)))
      (if (joga_probabilidades prob_M pa) ;o evento deve ocorrer?
          (if (> albedo_mae condicao) 
              (let (( albedo (- albedo_mae (* (/ 1 3) (- albedo_mae condicao))))) ;calcula e guarda o albedo calculado
                (cond ((< albedo 0 ) 0) ;verifica se o albedo calculado pertence a [0,1],devendo retornar 0 ou 1 caso ultrapasse
                      ((> albedo 1 ) 1)
                      (else albedo))) ;retorna o albedo calculado se for valido
              (let (( albedo (+ albedo_mae (* (/ 1 3) (- condicao albedo_mae))))) ;calcula e guarda o albedo calculado
                (cond((< albedo 0 ) 0) ;verifica se o albedo calculado pertence a [0,1],devendo retornar 0 ou 1 caso ultrapasse
                     ((> albedo 1 ) 1)
                     (else albedo)))) ;retorna o albedo calculado se for valido
          albedo_mae) ;não existe probabilidade de mutação, logo retorna o albedo da mae
      erro-input-novo-albedo)) ;o input dado é incorrecto

;2)a
(define (vizinhos_lista posicao H V ordem)
  
  (define (Hinicial posicao H V)
    (if (>= (- (linha_posicao posicao) ordem) 0) ;a linha pertence ao planeta?
        (- (linha_posicao posicao) ordem) ;como pertence, deverá ficar esta como inicial
        0)) ;nao pertence, deverá passar a ser a linha 0, visto ser a primeira do planeta
  
  (define (Hfinal posicao H V)
    (if (< (+ (linha_posicao posicao) ordem) H) ;a linha pertence ao planeta?
        (+ (linha_posicao posicao) ordem) ;nao excede, logo deverá ser a linha final
        (- H 1))) ;como excedeu deverá ser a ultima linha do planeta
  
  (define (Vinicial posicao H V)
    (if (>= (- (coluna_posicao posicao) ordem) 0) ;a coluna pertence ao planeta?
        (- (coluna_posicao posicao) ordem) ;pertence, logo esta eh valida
        0)) ;a coluna nao pertence ao planeta, logo deverá ser a primeira coluna do planeta
  
  (define (Vfinal posicao H V)
    (if (< (+ (coluna_posicao posicao) ordem) V) ;a coluna pertence ao planeta?
        (+ (coluna_posicao posicao) ordem) ;como pertence, eh valida
        (- V 1))) ;como nao pertence, sera considerada a ultima coluna do planeta
  
  (define (aux posicao H V ordem l c lista)
    (if (<= l (Hfinal posicao H V)) ; a linha actual eh inferior ou igual a linha final?
        (if (<= c (Vfinal posicao H V)) ; a linha actual eh inferior ou igual a coluna final?
            ;acrescenta a nova posicao a lista das posicoes que constituel a vizinhança
            (append (list(nova_posicao l c)) (aux posicao H V ordem l (+ c 1) lista))  ;muda de coluna
            (aux posicao H V ordem (+ l 1) (Vinicial posicao H V) lista)) ;muda de linha, e retorna a coluna ao valor inicial
        lista)) ;terminou de verificar, retorna a lista com a vizinhança
  
  (if (and (posicao? posicao) ;verificação dos argumentos
           (integer? H) (>= H 0)
           (integer? V) (>= V 0)
           (integer? ordem) (>= ordem 0))
      (if (or (= H 0) (= V 0) ;se a linha ou a coluna forem 0 não existe posiçoes, logo não existe vizinhança
              (not (posicao_valida? posicao H V))) ;posicao invalida -> erro de consistencia
          erro-consistencia-vizinhos-lista
          (aux posicao H V ordem (Hinicial posicao H V) (Vinicial posicao H V) ()))
      erro-input-vizinhos-lista)) ;o input dado é incorrecto

;2)b - quantas_vizinhas
(define (quantas_vizinhas planeta posicao ordem)
  
  (define (conta-margaridas lista-posicoes index size) ;conta o nº de margaridas vendo se cada posição da vizinhança é uma margarida
    (if (= index size)
        0
        (if (tem_margarida? planeta (list-ref lista-posicoes index))
            (+ 1 (conta-margaridas lista-posicoes (+ 1 index) size))
            (conta-margaridas lista-posicoes (+ 1 index) size))))
  
  
  (if (and (planeta? planeta) 
           (posicao? posicao) 
           (integer? ordem) (>= ordem 0)) ;verificaçao do input
      (if (not (posicao_valida? posicao (linhas_planeta planeta)(colunas_planeta planeta))) ;posição inválida -> erro de consistencia
          erro-consistencia-vizinhos-lista
          (if (= (colunas_planeta planeta) 0) ;se o planeta for vazio
              0
              (let ((lista-vizinhos (vizinhos_lista posicao (linhas_planeta planeta)(colunas_planeta planeta) ordem)))
                (conta-margaridas lista-vizinhos 0 (length lista-vizinhos))))) 
      ;se o planeta nao for vazio, retorna o tamanho da vizinhança recorrendo ao procedimento vizinhos_lista
      (cond ((erro-de-continuacao? planeta) planeta)
            ((erro-de-continuacao? posicao) posicao)
            (else erro-input-vizinhos-lista))));o input dado é incorrecto

;2)c - albedo de um planeta ou albedo de uma regiao
(define (alb_regiao . args) ;como pode receber um ou mais parametros, sera usado argumentos variaveis
  (if (= (length args) 1) ;deverá calcular o albedo do planeta inteiro
      (if (planeta? (list-ref args 0)) ;verifica se o planeta é valido
          (if (= (linhas_planeta (list-ref args 0)) 0) ; se o planeta for vazio, retorna o albedo do planeta
              erro-consistencia-alb-regiao
              (calcula_albedo (vizinhos_lista (nova_posicao 0 0) 
                                              (linhas_planeta (list-ref args 0))
                                              (colunas_planeta (list-ref args 0))
                                              (max (linhas_planeta (list-ref args 0))(colunas_planeta (list-ref args 0))))
                              (albedo_planeta (list-ref args 0)) ;o planeta é valido, irá calcular o albedo
                              (list-ref args 0)))
          (if (erro-de-continuacao? (list-ref args 0))
              (list-ref args 0)
              erro-input-alb-regiao))  ;o input dado é incorrecto
      (if (and (= (length args) 3)
               (planeta? (list-ref args 0))
               (integer? (list-ref args 1)) (>= (list-ref args 1) 0)
               (posicao? (list-ref args 2)));verificação do input dado
          (if (= (linhas_planeta (list-ref args 0)) 0) ; se o planeta for vazio, retorna o albedo do planeta
              erro-consistencia-alb-regiao
              ;o input é valido, irá calcular o albedo, região é dada pelo procedimento vizinhas_lista
              (let ((lista-posicoes (vizinhos_lista (list-ref args 2) ;posicao
                                                    (linhas_planeta (list-ref args 0)) ;linhas do planeta
                                                    (colunas_planeta (list-ref args 0)) ;colunas do planeta
                                                    (list-ref args 1))))
                (if (erro-de-continuacao? lista-posicoes) ;rebentou em algum lado?
                    erro-consistencia-alb-regiao
                    (calcula_albedo lista-posicoes
                                    (albedo_planeta (list-ref args 0)) ;albedo do planeta
                                    (list-ref args 0)))))
          (cond ((and (= (length args) 3) (erro-de-continuacao? (list-ref args 0))) (list-ref args 0))
                ((and (= (length args) 3) (erro-de-continuacao? (list-ref args 1))) (list-ref args 1))
                ((and (= (length args) 3) (erro-de-continuacao? (list-ref args 2))) (list-ref args 2))
                (else
                 erro-input-alb-regiao)))));o input dado é incorrecto

;2)d 
(define (conforto_margarida t planeta ordem posicao)
  (if (and (integer? t) (>= t 0) ;verifica os inputs dados
           (planeta? planeta) ;verifica se o planeta é valido
           (integer? ordem) (>= ordem 0) ;verifica se a ordem é valida
           (posicao? posicao)) ;verifica se a posicao é valida
      (if (not (posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)))
          erro-consistencia-conforto-margarida
          (if (tem_margarida? planeta posicao) ;verifica se o planeta tem uma margarida na posicao dada
              (let ((confortos 
                     (- 1 (* (/ 4 ;calcula e guarda o conforto calculado
                                (quadrado (- (tempmax_margarida (obtem_margarida planeta posicao))
                                             (tempmin_margarida (obtem_margarida planeta posicao)))))
                             (quadrado (- (Ti (tempmin_margarida (obtem_margarida planeta posicao))
                                              (tempmax_margarida (obtem_margarida planeta posicao)))
                                          (Tloc (alb_regiao planeta ordem posicao)
                                                (albedo_margarida (obtem_margarida planeta posicao)) t planeta)))))))
                (cond ((< confortos -1) -1) ;se o conforto for menor que -1, dará -1
                      ((> confortos 1) 1) ;se o conforto calculado for maior que 1, dará 1 
                      (else confortos))) ;senão retorna o conforto calculado
              erro-consistencia-conforto-margarida))
      (cond ((erro-de-continuacao? planeta) planeta)
            ((erro-de-continuacao? posicao) posicao)
            (else erro-input-conforto-margarida))))  ;input dado é incorrecto


;--------------------------------;
;       3ª Fase DaisyWorld       ;
;--------------------------------;

;Nome global que contém o procedimento que calcula a luminosidade de um planeta
(define formula-luminosidade lumino)

;(a) Devolve uma posição onde deverá nascer uma margarida 
(define (nasce_global t_ant planeta_ant posicao_mae)
  (define (percorre planeta_ant H V posicao_gerada h v flag)   ;com uma flag temos a certeza que já voltamos ao inicio a percorrer o mundo
    (if (and (= flag 1) (= h (linha_posicao posicao_gerada) ) (= v (coluna_posicao posicao_gerada)))
        posicao_mae ;não há posições livres
        (if (< h H)
            (if (< v V) 
                (if (tem_margarida? planeta_ant (nova_posicao h v)) ;se tiver margarida passa para a próxima posição
                    (percorre planeta_ant H V posicao_gerada h (+ v 1) flag) ;coluna seguinte
                    (nova_posicao h v)) ;senao devolve a posição actual
                (percorre planeta_ant H V posicao_gerada (+ h 1) 0 flag)) ;linha seguinte 
            (percorre planeta_ant H V posicao_gerada 0 0 1)))) ;volta a percorrer a partir do inicio
  
  ;validação dos argumentos
  (cond ((erro-de-continuacao? planeta_ant) planeta_ant)
        ((erro-de-continuacao? posicao_mae) posicao_mae)
        ((not (and (integer? t_ant) (>= t_ant 0) (planeta? planeta_ant) (posicao? posicao_mae))) erro-input-nasce-global)
        ((or (= (linhas_planeta planeta_ant) 0) 
             (not(posicao_valida? posicao_mae (linhas_planeta planeta_ant) (colunas_planeta planeta_ant)))
             (not (tem_margarida? planeta_ant posicao_mae)))
         erro-consistencia-nasce-global) ;se o planeta for vazio, ou se a posição dada nao contiver uma margarida ou nao for válida.-> erro de consistencia
        (else 
         (let* ((H (linhas_planeta planeta_ant)) ;Dimensoes planeta
                (V (colunas_planeta planeta_ant))
                (n-aleatorio (gerador "aleatorio" t_ant (linha_posicao posicao_mae) (coluna_posicao posicao_mae))) ;gera nº aleatorio
                (posicao-gerada (posicao_a_sorte H V n-aleatorio)) ;posicao pseudo-aleatória
                (h (linha_posicao posicao-gerada)) 
                (v (coluna_posicao posicao-gerada)))
           (if (tem_margarida? planeta_ant posicao-gerada) ; se a posição não estiver vazia, devolve a mesma, senao procura a próxima vazia 
               (percorre planeta_ant H V posicao-gerada h (+ v 1) 0)
               posicao-gerada) ;se já houver uma margarida na posição gerada, devolve essa posição
           ))))

;(b)
(define (nasce_local t planeta posicao ordem)
  
  (define (percorre i planeta posicao lista)  ;percorre a lista com as posições da vizinhança
    (if (< i (length lista))
        (if (tem_margarida? planeta (list-ref lista i))
            (percorre (+ i 1) planeta posicao lista) ;próxima posição
            (list-ref lista i)) ;se nao houver margarida, entao eh nesta posição que irá nascer
        posicao)) ;devolve a posicao que se encontra vazia
  
  (cond ((erro-de-continuacao? planeta) planeta) ;Validação dos argumentos
        ((erro-de-continuacao? posicao) posicao)
        ((not (and (integer? t) (>= t 0) (integer? ordem) (>= ordem 0) (planeta? planeta) (posicao? posicao))) erro-input-nasce-local) 
        ((or (= (linhas_planeta planeta) 0) 
             (not(posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta)))
             (not (tem_margarida? planeta posicao)))
         erro-consistencia-nasce-local) ;erro de consistencia caso a posicao nao seja valida, ou o planeta nao ter posições, ou nao houver nenhuma margarida na posição dada
        (else (percorre 0 planeta posicao (vizinhos_lista posicao (linhas_planeta planeta) (colunas_planeta planeta) ordem)))))

;(c) ;Determina o que acontece com uma margarida
(define (efeito_margarida t planeta posicao ordem Vm)
  
  
  (define (efeito t planeta conforto posicao Vm)
    (cond ((< conforto 0) (if (joga_probabilidades (- 0 conforto) 
                                                   (gerador "aleatorio" t (linha_posicao posicao) (coluna_posicao posicao))) 
                              "morre" 
                              "nada")) ;se o conforto for entre -1 e 0, vemos se a margarida morre
          ((< (quantas_vizinhas planeta posicao ordem) Vm) ;verifica se a margarida puderá nascer na vizinhança
           (if (joga_probabilidades 1 (gerador "aleatorio" t (linha_posicao posicao) (coluna_posicao posicao))) 
               (let ((nascimento (nasce_local t planeta posicao ordem))) ;se a posição dada for igual á da mae, entao nada acontece
                 (if (and (= (linha_posicao posicao) (linha_posicao nascimento)) ; senao ela vai nascer na posição calculada
                          (= (coluna_posicao posicao) (linha_posicao nascimento)))
                     "nada"
                     nascimento))
               "nada"))
          ;caso contrário vemos se irá de facto nascer numa posição aleatoria do planeta
          (else (if (joga_probabilidades conforto (gerador "aleatorio" t (linha_posicao posicao) (coluna_posicao posicao))) 
                    (nasce_global t planeta posicao) ;se o evento deve ocorrer, ela nasce, senao nada acontece
                    "nada"))))
  
  (cond ((erro-de-continuacao? planeta) planeta) ;validação dos inputs.
        ((erro-de-continuacao? posicao) posicao)
        ((not(and (integer? t) (>= t 0) (planeta? planeta) (posicao? posicao)
                  (integer? ordem) (>= ordem 0) (integer? Vm) (>= Vm 0)))
         erro-input-efeito-margarida)
        ((or (= (linhas_planeta planeta) 0) 
             (not(posicao_valida? posicao (linhas_planeta planeta) (colunas_planeta planeta))) 
             (not (tem_margarida? planeta posicao))) ;verifica se a posicao dada é válida, contém uma margarida e se o planeta tem posições. 
         erro-consistencia-efeito-margarida)
        (else (efeito t planeta (conforto_margarida t planeta ordem posicao) posicao Vm))))

;(d) Devolve um planeta (rep. interna) com a alteração referente ao que deve acontecer a uma margarida 
(define (aplica_margarida t p_ant posicao ordem Vm pM p_seg)
  (cond ((erro-de-continuacao? p_ant) p_ant)
        ((erro-de-continuacao? p_seg) p_seg)
        (else                                  ;validação dos inputs
         (if (erro-de-continuacao? posicao) 
             posicao
             (if (not (and (integer? t) (>= t 0) 
                           (planeta? p_ant) 
                           (posicao? posicao)
                           (integer? ordem) (>= ordem 0) 
                           (integer? Vm) (>= Vm 0) 
                           (rational? pM) (>= pM 0) (<= pM 1)
                           (planeta? p_seg)
                           )) 
                 erro-input-aplica-margarida
                 (if (or (not(posicao_valida? posicao (linhas_planeta p_ant) (colunas_planeta p_ant))) 
                         (not (tem_margarida? p_ant posicao))
                         (= (linhas_planeta p_ant) 0)
                         (not (= (albedo_planeta p_ant) (albedo_planeta p_seg))) 
                         (not (= (linhas_planeta p_ant) (linhas_planeta p_seg)))
                         (not (= (colunas_planeta p_ant) (colunas_planeta p_seg)))) ;a posicao dada tem que ser valida e tem que ter uma margarida
                     erro-consistencia-aplica-margarida           ;Acabou a validação dos inputs bem como a consistência dos mesmos
                     (let ((efeito-p-anterior (efeito_margarida t p_ant posicao ordem Vm))) 
                       (cond ((equal? efeito-p-anterior "morre") ;Sabemos agora o que acontece com a margarida
                              (morre_margarida p_seg posicao)) ;se morrer, devolvemos o mundo sem a mesma
                             ((equal? efeito-p-anterior "nada") ;se nada acontecer, basta devolver o mesmo mundo
                              p_seg)
                             (else 
                              (if (posicao? efeito-p-anterior) ;se for uma posicao ---------------> ou é a da mae e nada deve acontecer ou é outra e deve nascer
                                  (if (and (= (linha_posicao posicao)  (linha_posicao efeito-p-anterior)); na mesma (posição).
                                           (= (coluna_posicao posicao) (coluna_posicao efeito-p-anterior)))
                                      p_seg
                                      (nasce_margarida p_seg 
                                                       efeito-p-anterior 
                                                       (nova_margarida (novo_albedo (albedo_margarida (obtem_margarida p_ant posicao)) ;faz nascer com o albedo da 
                                                                                    pM ;                                    mãe ou com um novo albedo, resultante
                                                                                    (gerador "aleatorio" t ;                se deve ou nao haver mutação
                                                                                             (linha_posicao posicao) 
                                                                                             (coluna_posicao posicao)))
                                                                       (tempmin_margarida (obtem_margarida p_ant posicao))
                                                                       (tempmax_margarida (obtem_margarida p_ant posicao)))))
                                  erro-consistencia-aplica-margarida))))))))))
  
  ;Executa simulação do modelo computacional apresentado no enunciado "Aqui começa o programa" 
  (define (simula planeta . args)
    
    (define (ciclo t_actual t_ant t_max ordem planeta planeta_seg Vm pM trace albedo outputs h v)
      (if (<= t_actual t_max) ; percorre até ao tempo máximo 
          (if (< h (linhas_planeta planeta)) ;percorremos linha a linha, coluna a coluna
              (if (< v (colunas_planeta planeta))
                  (if (tem_margarida? planeta (nova_posicao h v)) ;se tiver margarida, vamos processá-la (matá-la, nascer uma filha, ou nada)
                      (ciclo t_actual t_ant t_max ordem planeta
                             (aplica_margarida t_ant planeta (nova_posicao h v) ordem Vm pM planeta_seg)
                             Vm pM trace albedo outputs h (+ v 1))
                      (ciclo t_actual t_ant t_max ordem planeta planeta_seg Vm pM trace albedo outputs h (+ v 1))) ;vamos 
                  (ciclo t_actual t_ant t_max ordem planeta planeta_seg Vm pM trace albedo outputs (+ h 1) 0))
              
              (begin (if (equal? trace "on") ;se tivermos sempre que ter todos os outputs na lista final de outputs o trace ta a on e assim
                         (set! outputs (append outputs (list (list t_actual (intern_to_extern planeta_seg) albedo))))) ;adicionamos o output actual 
                     ;                                                                                                    á lista
                     (if (or (= t_actual t_max)
                             (and (= (quantas_vizinhas planeta_seg (nova_posicao 0 0) (max (linhas_planeta planeta) (colunas_planeta planeta))) 0)
                                  (not (tem_margarida? planeta_seg (nova_posicao 0 0))))) ;se nao houverem margaridas no mundo ou se o tempo eh 
                         (if (equal? trace "off") ;                                        o tempo final acabamos a recursão 
                             (append outputs (list (list t_actual (intern_to_extern planeta_seg) albedo))) ;e devolvemos a lista de outputs
                             outputs) ;senao continua o ciclo ate chegar ao instante final.
                         (ciclo (+ t_actual 1)(+ t_ant 1) t_max ordem planeta_seg (copia_planeta planeta_seg) Vm pM trace albedo outputs 0 0))))))
    
    
    
    (let ((simulacao (nova-simulacao planeta args))) ;Cria uma nova simulação, o resultado pode ser correcto ou pode ser devolvido um erro de utilização
      (cond ((erro-de-continuacao? planeta) planeta) ;Faz a validação dos inputs.
            ((not (string? planeta)) "#simula: Erro de input." )
            ((not (planeta? (extern_to_intern planeta 0.5))) "#simula: Erro de input.")
            ((equal? simulacao "Erro de utilização.") "#simula: Erro de utilização.")
            (else
             (let* ((t_actual (+ (simulacao-init simulacao) 1)) ;Inicializa variáveis que irão ser necessárias: tempo actual;
                    (t_ant (- t_actual 1)) ;tempo anterios;
                    (t_max (+ (simulacao-steps simulacao) t_ant)) ;tempo final (calculado a partir dos steps)
                    (ordem (simulacao-vizinhanca simulacao)) ;ordem da vizinhança
                    (planeta (simulacao-planeta simulacao)) ; o planeta
                    (planeta_seg (copia_planeta planeta)) ; a copia do planeta
                    (Vm (VM (simulacao-densidade simulacao) ordem)) ;o limite Vm
                    (trace (simulacao-trace simulacao)) ; guarda a informação do trace da simulação
                    (albedo (simulacao-albedo simulacao)) ;albedo do planeta da simulação
                    (p_mutacao (simulacao-mutate simulacao)) ;probabilidade de mutação
                    (luminosidade (simulacao-lumino simulacao)) ;procedimento para calcular a luminosidade solar
                    (semente (simulacao-seed simulacao))) ;semente para inicializar o gerador de nºs aleatórios
               (if (not (equal? luminosidade "lumino"))
                   (set! formula-luminosidade luminosidade)) ;muda a formula da luminosidade
               (begin (gerador "inicializa" semente (linhas_planeta planeta) (colunas_planeta planeta)) ;inicia gerador para a simulacao
                      (let ((a (ciclo t_actual t_ant t_max ordem planeta planeta_seg Vm p_mutacao trace albedo null 0 0))) ;calcula a lista de outputs
                        (begin (gerador "termina") ;reset ao gerador
                               (set! formula-luminosidade lumino) ;repoe a fórmula "default" da luminosidade solar
                               a)))))))) ;no final devolve a lista com os outputs.