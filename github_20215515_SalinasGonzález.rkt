#lang racket
;Se define función git que recibirá los comandos
(define git (lambda (x) (cond ;se identifica y se ejecuta la función identificada
                           ((equal? x "pull") pull)
                           ((equal? x "add") add)
                           ((equal? x "commit") commit)
                           ((equal? x "push") push))))


(define append (lambda (listOne listTwo)
  (if (null? listOne)
       listTwo
      (cons (car listOne) (append (cdr listOne) listTwo))
      )
  ))




(define comprobarExistencia (lambda (x lista) (if(null? lista) #f (if (equal? x (car lista)) #t (comprobarExistencia x (cdr lista))))))


(define comprobarAdd (lambda (repositorio lista) (if (null?  lista) #t (if (comprobarExistencia (car lista) repositorio) (comprobarAdd repositorio (cdr lista) ) #f) )))



;Se define la función add
(define add (lambda (archivos zona) (if(list? archivos) (if(comprobarAdd (list-ref zona 0) archivos) (list (list-ref zona 0) (append (list-ref zona 1) archivos) (list-ref zona 2) (list-ref zona 3) (list-ref zona 4)  ) zona) zona) ))

;Se define la función commit
(define commit (lambda (nombreArchivo) (nombreArchivo) ))
;Se define la función push
(define push (lambda (nombreArchivo) (nombreArchivo) ))
;Se define la función pull
(define pull (lambda (nombreArchivo) (nombreArchivo) ))


;El TDA de workSpace se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de index se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de localRepository se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de remoteRepository se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.


;Se define generador de ID
(define generadorID (lambda (nombreArchivo)(list nombreArchivo (random 100))))

(require racket/date)
(define fecha (lambda () (date->string (current-date) second)))

(define obtenerDatos (lambda (datos) (if(string? datos) (list datos (fecha)) null)))

(define datos? (lambda x (if(list? x) (if(and (string?(car(cdr x))) (string?(car x) (null?(cdr (cdr x))))) #t #f) #f )     ))


;TDA Zonas : Es una lista de TDA's donde cada uno de los argumentos representa cada una de las zonas donde se traspasa la información
(define zonas (lambda (workSpace index localRepository remoteRepository datos comando) (if(and(list? workSpace);TDA workSpace:  Consta de una lista que contiene los nombres de los archivos presentes en el directorio local del PC y sus id respectivas
                                                                                (list? index); TDA index: Consta de una lista
                                                                                (list? localRepository); TDA Local Repository : Consta de una lista que contiene id - fecha y hora - nombre de realizador - comentario - listado de archivos del commit - id version anterior
                                                                                (list? remoteRepository);TDA workSpace :
                                                                                (datos? datos)
                                                                                (string? comando)
                                                                                )(list workSpace index localRepository remoteRepository datos comando);retorno si es que todos son listas
                                                                                 null);retorno si uno de los argumentos no es lista
                                                                                ))
