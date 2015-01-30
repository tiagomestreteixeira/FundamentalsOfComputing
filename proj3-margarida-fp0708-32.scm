(require (lib "include.ss"))
(include "proj3-aux-fp0708-32.scm")

;Tipo Abstracto de Informação -> Margarida

;Definição das operações básicas

;Representação interna -> lista com 3 posições em que a primeiro é o albedo, a segunda a temp. mínima de sobre. e a terceira a 
;                         temp. máxima de sobre. (A definição é feita a partir das listas primitivas) 


;nova_margarida : [0, 1] × Inteiro × Inteiro → Margarida ∪ String
(define (nova_margarida alb t-min t-max)
  (if (and (and (rational? alb)  (>= alb 0) (<= alb 1))
           (integer? t-min)
           (integer? t-max))    ;validação dos 3 argumentos recebidos  
      (if (> t-max t-min)       ;verifica se existe um erro de consistência
          (list alb t-min t-max);se não existir, constrói uma margarida (lista).
          erro-consistencia-nova-margarida) ;erro de consistência
      erro-input-nova-margarida)) ;erro de input


;albedo_margarida : Margarida → [0, 1] ∪ String
;Seleciona o albedo de uma margarida
(define (albedo_margarida marg)
  (if (margarida? marg)     ;Se o argumento for uma margarida, devolve o primeiro elemento da lista
      (list-ref marg 0)                   
      (if (erro-de-continuacao? marg)
          marg
          erro-input-albedo-margarida)))                    


;tempmin_margarida : Margarida → Natural ∪ String
(define (tempmin_margarida marg) ;Seleciona a temperatura mínima de sobrevivência de uma Margarida
  (if (margarida? marg)                         
      (list-ref marg 1);       Se o argumento for uma margarida, devolve o segundo elemento da lista, senão : 
      (if (erro-de-continuacao? marg)
          marg
          erro-input-tempmin-margarida)));         erro de input.


;tempmax_margarida : Margarida → Natural ∪ String
(define (tempmax_margarida marg); ;Seleciona a temperatura máxima de sobrevivência de uma Margarida
  (if (margarida? marg);                           
      (list-ref marg 2);                   Se o argumento for uma margarida, devolve o terceiro elemento da lista, senão :
      (if (erro-de-continuacao? marg)
          marg
          erro-input-tempmax-margarida)));                     erro de input



;tempideal_margarida : Margarida → Racional ∪ String
(define (tempideal_margarida marg) ;Calcula a temperatura ideal de proliferação de uma Margarida
  (if (margarida? marg)                          
      (Ti (list-ref marg 1) (list-ref marg 2)) ;Se o argumento for uma margarida, vai calcular a Temp. idela de proliferação, senão :
      (if (erro-de-continuacao? marg)
          marg
          erro-input-tempideal-margarida))) ;                         erro de input. 



;margarida? : Universal -> lógico
(define (margarida? arg) ;Verifica se o argumento recebido é de facto uma margarida e para isso tem que cumprir :
   
  (and (list? arg) ;Tem que ser uma lista
       (= (length arg) 3)
       (let ((alb-marg (list-ref arg 0)) ;alb-mar = albedo da margarida que se encontra na posição 0 da lista 
             (t-min (list-ref arg 1))    ;t-min = temp. min. de sobrevivencia da margarida que se encontra na posição 1 da lista 
             (t-max (list-ref arg 2)))   ;t-max = temp. max. de sobrevivencia da margarida que se encontra na posicao 2 da lista
         (and (rational? alb-marg)   ;Validação do albedo 
              (>= alb-marg 0) 
              (<= alb-marg 1) 
              (integer? t-min) ;Validação da temperatura minima de sobrevivência 
              (integer? t-max) ;Validação da temperatura máxima de sobrevivência
              (> t-max t-min)))))  ;a temp. máxima tem que ser maior que a temperatura minima, senao temos um caso de inconsistencia

;imprime_margarida: Margarida → String
(define (imprime_margarida marg);Transformador de saída para a representação externa pedida deste TAI : "alb,t_min,t_max"
  (if (margarida? marg);                             
      (string-append (number->string (list-ref marg 0)) ;Se o argumento for válido é formada uma string de acordo com a representação
                     ","                                ;externa. Senão "Erro de input."
                     (number->string (list-ref marg 1))
                     ","
                     (number->string (list-ref marg 2)))
      (if (erro-de-continuacao? marg)
          marg
          erro-input-imprime-margarida)))
