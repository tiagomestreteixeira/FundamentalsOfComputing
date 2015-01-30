(require (lib "include.ss"))
(require "gerador.zo")
(require "tokenize.zo")
(require "interface.zo")

;;;
;;; Devem incluir todos os ficheiros indicados no enunciado e que tenham realizado
;;; O nome correcto dos ficheiros e dos procedimentos é fundamental para a avaliação do vosso trabalho
;;; NÃO DEVEM INCLUIR O FICHEIRO PROJ2-PLANETA-FP0708-XX.SCM (implementação sobre listas)!!! 
;;;   Este deverá estar apenas no ficheiro .zip
;;;
;;; Os ficheiros não devem produzir nenhum output. Devem apenas conter as definições dos procedimentos/constantes
;;; Quaisquer testes que realizem durante o desenvolvimento do projecto têm de ser comentados ou removidos antes 
;;;   da entrega sob pena de gerarEM resultados errados durante o processo de avaliação
;;;
;;; NÃO PODEM SER USADOS MÓDULOS! A utilização destes poderá levar a uma avaliação incorrecta do vosso trabalho
;;;
;;; RELEMBRAMOS QUE O RESULTADO DO SIMULADOR É UMA LISTA DE RESULTADOS CONFORME ESPECIFICADO NO ENUNCIADO
;;; A FUNÇÃO play_daisyworld NÃO DEVERÁ SER USADA DENTRO DO SIMULADOR!
;;;

(include "proj3-aux-fp0708-32.scm")
(include "proj3-margarida-fp0708-32.scm")
(include "proj3-posicao-fp0708-32.scm")
;;; relembramos que o ficheiro proj3-planeta-fp0708-xx.scm é a implementação do tipo planetas sobre vectores
(include "proj3-planeta-fp0708-32.scm")
(include "proj3-simulador-fp0708-32.scm")
(include "proj3-procs-fp0708-32.scm")
