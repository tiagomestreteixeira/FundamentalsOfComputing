(require (lib "include.ss"))
(include "proj3-aux-fp0708-32.scm")

;Tipo Abstracto de Informação: Posicao
;Representação interna escolhida : Uma posição é um par, em que no 1º elemento encontra-se a coordenada
;                                  referente á linha e o 2º elemento contém a coordenada referente á coluna.


; nova_posicao : Natural × Natural → Posição ∪ String
(define (nova_posicao H V)
  (if (and (natural? H)   ;Validação dos Argumentos
           (natural? V))
      (cons H V)          ;constrói uma nova posição
      erro-input-nova-posicao))

; linha_posicao : Posicao → Natural ∪ String 
(define (linha_posicao posicao)
  (if (posicao? posicao)          ;Verificamos se o argumento recebido foi de facto uma posição
      (car posicao)     ;Devolve a linha
      (if (erro-de-continuacao? posicao)
          posicao
          erro-input-linha-posicao)))

; coluna_posicao : Posicao → Natural ∪ String
(define (coluna_posicao posicao)
  (if (posicao? posicao)      ;Validação do argumento
      (cdr posicao)    ;Devolve a coluna
      (if (erro-de-continuacao? posicao)
          posicao
          erro-input-coluna-posicao)))

; posicao_valida? : Posicao × Natural × Natural → Natural ∪ String 
(define (posicao_valida? posicao H V)
  (if (and (posicao? posicao)          ;Verifica se recebeu uma posição
           (natural? H)                ;Verifica se as coordenadas são númenos naturais
           (natural? V))
      (and (< (car posicao) H)         ;Verifica se o nº de linhas e o nº de colunas se encontra dentro dos parâmetros estabelecidos.
           (< (cdr posicao) V))
      (if (erro-de-continuacao? posicao)
          posicao
          erro-input-posicao-valida?)))

; posicao? : Universal → Lógico
(define (posicao? posicao)    
  (and (pair? posicao)          ;Uma posição segundo a representação escolhida, tem que ser um par com
       (natural? (car posicao)) ;nºs naturais em cada posição.
       (natural? (cdr posicao))))
