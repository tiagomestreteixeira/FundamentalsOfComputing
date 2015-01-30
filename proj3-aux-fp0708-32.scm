;Mensagens de Erro
(define erro-de-input "Erro de input.") ;erro de input
(define erro-de-consistencia "Erro de consistência.") ;erro de consistencia


;Predicado natural? : Universal -> Booleano
(define (natural? arg)
  (and (integer? arg) (>= arg 0)))

;constantes:
(define FHA 20) ;factor absorcao
(define E 0.5) ;emissividade do planeta
(define SB  (* 5.67 (expt 10 -8))) ;stefan-boltzmann
(define FS 917) ;fluxo solar



;Temperatura Local
(define (Tloc albedo_regiao albedo_margarida t planeta)
  (+ (* FHA (- albedo_regiao albedo_margarida)) (Tp planeta t)))


;Temperatura ideal de proliferação
(define (Ti Tm TM)
  (/ (+ Tm TM)
     2))


;Calcula o quadrado de um número
(define (quadrado x)
  (* x x))


; Percorre o planeta para obter os dados necessarios para calcular o albedo  
(define (calcula_albedo lista alb_planeta planeta)
  (define (percorre planeta alb_planeta lista marg n_marg n_vazias vazias i) ;o planeta irá ser percorrido  
    (if (null? lista) ;se chegar ao fim vai finalmente fazer o calculo final (acabou de recolher os dados essenciais para a fórmula)
        (calcula marg n_marg n_vazias vazias alb_planeta)
        (if (tem_margarida? planeta 
                            (list-ref lista 0)) ;posição actual contém uma margarida?
            (percorre planeta alb_planeta (list-tail lista 1) ;próxima posição 
                      (+ marg (albedo_margarida (obtem_margarida planeta (list-ref lista 0)))) ; soma o alb da margarida da posição act.
                      (+ n_marg 1) n_vazias vazias (+ i 1))  ;se a posição não contiver uma margarida entao é uma posição vazia
            (percorre planeta alb_planeta (list-tail lista 1) marg n_marg (+ n_vazias 1) (+ vazias alb_planeta) (+ i 1)))))
                                                             ;soma o alb do planeta e incrementa o nº de posições vazias
  
  (percorre planeta alb_planeta lista 0 0 0 0 0))

;aplica a formula que permite obter o albedo final
(define (calcula marg n_marg n_vazias vazias albedo_planeta)
  
  (if (= n_marg 0) ;se no planeta não constarem margaridas, o albedo eh apenas o das posições vazias
      (let ((alb (/ vazias n_vazias)))
        (cond (( < alb 0) 0)
              (( > alb 1) 1)
              (else alb)))
      (if (= n_vazias 0) ;se não houverem posições vazias, o albedo eh apenas o das margaridas 
          (let ((alb (/ marg n_marg)))
            (cond ((< alb 0) 0) ;se o albedo calculado for menor que zero, dará zero
                  ((> alb 1) 1) ;se o albedo calculado for maior que um, dará um
                  (else alb)))     
          (let ((alb (/ (+ marg vazias) (+ n_marg n_vazias)))) ;calcula o albedo e guarda o valor
            (cond ((< alb 0) 0) ;se o albedo calculado for menor que zero, dará zero
                  ((> alb 1) 1) ;se o albedo calculado for maior que um, dará um
                  (else alb)))))) ;o albedo é valido logo deverá retornar o albedo calculado

;Predicado que verifica se um vector se encontra vazio
(define (vector-empty? v)
  (and (vector? v)
       (= (vector-length v) 0)))

;Adaptação do append usado em listas para vectores. De notar que o procedimento apenas trabalha com 2 vectores passados como argumentos
(define vector-append 
  (lambda (v1 v2)
    (let* ((n1 (vector-length v1))
           (n2 (vector-length v2))
           (v3 (make-vector (+ n1 n2)))) ;novo vector com a dimensao = dimensao de v1 + dimensao de v2
      (vector-copy-n v1 0 v3 0 n1) ;copia os elementos do primeiro vector para v3
      (vector-copy-n v2 0 v3 n1 n2) ;copia os elementos do segundo vector para v3
      v3)))
        
; Copia n valores do vector v1 para o vector v2 starting at offset pos1 and pos2, resp.
(define (vector-copy-n v1 pos1 v2 pos2 n)
  (define (iter) 
    (when (> n 0)
      (vector-set! v2 pos2 (vector-ref v1 pos1)) ;memoriza posição actual no novo vector
      (set! pos1 (+ pos1 1)) ;incrementa posição do vector a ser copiado
      (set! pos2 (+ pos2 1)) ;incrementa posição do vector a ser preenchido
      (set! n (- n 1)) ; decrementa nº de vezes do ciclo
      (iter))) ;repete o ciclo
  (iter))

  
  ;Procedimento de ordem superior que devolve #t se todos os elementos de um vector satisfazem
  (define (todos-satisfazem? vec pred index size) ; o predicado passado no segundo argumento
    (if (= index size)
        #t
        (if (pred (vector-ref vec index)) ;O elemento actual satisfaz o predicado?
            (todos-satisfazem? vec pred (+ index 1) size) ;se sim entao vai ver os elementos seguintes
            #f)))

  
  ;verifica se uma string contém um determinado caracter
  (define (existe-char? ch str)
    (define (verifica index size)
      (if (= index size)
          #f
          (if (char=? ch (string-ref str index)) ;index -> indice de um caracter na string
              #t
              (verifica (+ 1 index) size))))
  
    (if (= (string-length str) 0)
        #f
      (verifica 0 (string-length str))))
  
  ;calcula o limite vm - nº de margaridas toleradas na vizinhança
  (define (VM densidade ordem)
    (truncate (* densidade (quadrado (+ 1 (* ordem 2))))))
  
  
;Constantes que contêm as strings dos erros especiais  
(define erro-input-nova-posicao "#nova_posicao: Erro de input.")
(define erro-input-linha-posicao "#linha_posicao: Erro de input.")
(define erro-input-coluna-posicao "#coluna_posicao: Erro de input.")
(define erro-input-posicao-valida? "#posicao_valida?: Erro de input.")
(define erro-input-nova-margarida "#nova_margarida: Erro de input.")
(define erro-consistencia-nova-margarida "#nova_margarida: Erro de consistência.")
(define erro-input-albedo-margarida "#albedo_margarida: Erro de input.")
(define erro-input-tempmin-margarida "#tempmin_margarida: Erro de input.")
(define erro-input-tempmax-margarida "#tempmax_margarida: Erro de input.")
(define erro-input-tempideal-margarida "#tempideal_margarida: Erro de input.")
(define erro-input-imprime-margarida "#imprime_margarida: Erro de input.")
(define erro-input-novo-planeta "#novo_planeta: Erro de input.")
(define erro-input-albedo-planeta "#albedo_planeta: Erro de input.")
(define erro-input-linhas-planeta "#linhas_planeta: Erro de input.")
(define erro-input-colunas-planeta "#colunas_planeta: Erro de input.")
(define erro-input-nasce-margarida "#nasce_margarida: Erro de input.")
(define erro-consistencia-nasce-margarida "#nasce_margarida: Erro de consistência.")
(define erro-input-morre-margarida "#morre_margarida: Erro de input.")
(define erro-consistencia-morre-margarida "#morre_margarida: Erro de consistência.")
(define erro-input-obtem-margarida "#obtem_margarida: Erro de input.")
(define erro-consistencia-obtem-margarida "#obtem_margarida: Erro de consistência.")
(define erro-input-tem-margarida? "#tem_margarida?: Erro de input.")
(define erro-consistencia-tem-margarida? "#tem_margarida?: Erro de consistência.")
(define erro-input-extern-to-intern "#extern_to_intern: Erro de input.")
(define erro-input-intern-to-extern "#intern_to_extern: Erro de input.")
(define erro-input-copia-planeta "#copia_planeta: Erro de input.")
(define erro-input-joga-probabilidades "#joga_probabilidades: Erro de input.")
(define erro-input-posicao-a-sorte "#posicao_a_sorte: Erro de input.")
(define erro-input-novo-albedo "#novo_albedo: Erro de input.")
(define erro-input-vizinhos-lista "#vizinhos_lista: Erro de input.")
(define erro-consistencia-vizinhos-lista "#vizinhos_lista: Erro de consistência.")
(define erro-input-quantas-vizinhas "#quantas_vizinhas: Erro de input.")
(define erro-consistencia-quantas-vizinhas "#quantas_vizinhas: Erro de consistência.")
(define erro-input-alb-regiao "#alb_regiao: Erro de input.")
(define erro-consistencia-alb-regiao "#alb_regiao: Erro de consistência.")
(define erro-input-conforto-margarida "#conforto_margarida: Erro de input.")
(define erro-consistencia-conforto-margarida "#conforto_margarida: Erro de consistência.")
(define erro-input-nasce-global "#nasce_global: Erro de input.")
(define erro-consistencia-nasce-global "#nasce_global: Erro de consistência.")
(define erro-input-nasce-local "#nasce_local: Erro de input.")
(define erro-consistencia-nasce-local "#nasce_local: Erro de consistência.")           
(define erro-input-efeito-margarida "#efeito_margarida: Erro de input.")
(define erro-consistencia-efeito-margarida "#efeito_margarida: Erro de consistência.")
(define erro-input-aplica-margarida "#aplica_margarida: Erro de input.")
(define erro-consistencia-aplica-margarida "#aplica_margarida: Erro de consistência.")    



;Verifica se determinado argumento é de facto um erro especial
(define (erro-de-continuacao? arg)
  (and (string? arg)
       (or (equal? arg erro-input-nova-posicao)
           (equal? arg erro-input-linha-posicao)
           (equal? arg erro-input-coluna-posicao)
           (equal? arg erro-input-posicao-valida?)
           (equal? arg erro-input-nova-margarida)
           (equal? arg erro-consistencia-nova-margarida)
           (equal? arg erro-input-albedo-margarida)
           (equal? arg erro-input-tempmin-margarida)
           (equal? arg erro-input-tempmax-margarida)
           (equal? arg erro-input-tempideal-margarida)
           (equal? arg erro-input-imprime-margarida)
           (equal? arg erro-input-novo-planeta)
           (equal? arg erro-input-albedo-planeta)
           (equal? arg erro-input-linhas-planeta)
           (equal? arg erro-input-colunas-planeta)
           (equal? arg erro-input-nasce-margarida)
           (equal? arg erro-consistencia-nasce-margarida)
           (equal? arg erro-input-morre-margarida)
           (equal? arg erro-consistencia-morre-margarida)
           (equal? arg erro-input-obtem-margarida)
           (equal? arg erro-consistencia-obtem-margarida)
           (equal? arg erro-input-tem-margarida?)
           (equal? arg erro-consistencia-tem-margarida?)
           (equal? arg erro-input-extern-to-intern)
           (equal? arg erro-input-intern-to-extern)
	   (equal? arg erro-input-copia-planeta)
           (equal? arg erro-input-joga-probabilidades)
           (equal? arg erro-input-posicao-a-sorte)
           (equal? arg erro-input-novo-albedo)
           (equal? arg erro-input-vizinhos-lista)
           (equal? arg erro-consistencia-vizinhos-lista)
           (equal? arg erro-input-quantas-vizinhas)
           (equal? arg erro-consistencia-quantas-vizinhas)
           (equal? arg erro-input-alb-regiao)
           (equal? arg erro-consistencia-alb-regiao)
           (equal? arg erro-input-conforto-margarida)
           (equal? arg erro-consistencia-conforto-margarida)
           (equal? arg erro-input-nasce-global)
           (equal? arg erro-consistencia-nasce-global)
           (equal? arg erro-input-nasce-local)
           (equal? arg erro-consistencia-nasce-local)
           (equal? arg erro-input-efeito-margarida)
           (equal? arg erro-consistencia-efeito-margarida)
           (equal? arg erro-input-aplica-margarida)
           (equal? arg erro-consistencia-aplica-margarida))))