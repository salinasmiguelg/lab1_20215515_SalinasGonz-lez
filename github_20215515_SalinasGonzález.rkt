#lang racket
;Se define función git que recibirá los comandos
(define git (lambda (x) (cond ;se identifica y se ejecuta la función identificada
                           ((equal? x "pull") pull)
                           ((equal? x "add") add)
                           ((equal? x "commit") commit)
                           ((equal? x "push") push))))

;Se define la función add
(define add (lambda (nombreArchivo) (nombreArchivo) ))
;Se define la función pull
(define pull (lambda (nombreArchivo) (nombreArchivo) ))
;Se define la función commit
(define commit (lambda (nombreArchivo) (nombreArchivo) ))
;Se define la función push
(define push (lambda (nombreArchivo) (nombreArchivo) ))

;Se define generador de ID
(define generadorID (lambda (nombreArchivo)(list nombreArchivo (random 100))))

;TDA Zonas : Es una lista de TDA's donde cada uno de los argumentos representa cada una de las zonas donde se traspasa la información
(define zonas (lambda (workSpace index localRepository remoteRepository) (if(and(list? workSpace);TDA workSpace:  Consta de una lista que contiene los nombres de los archivos presentes en el directorio local del PC y sus id respectivas
                                                                                (list? index); TDA index: Consta de una lista
                                                                                (list? localRepository); TDA Local Repository : Consta de una lista que contiene id - fecha y hora - nombre de realizador - comentario - listado de archivos del commit - id version anterior
                                                                                (list? remoteRepository);TDA workSpace :
                                                                                )(list workSpace index localRepository remoteRepository);retorno si es que todos son listas
                                                                                 null);retorno si uno de los argumentos no es lista
                                                                                ))
