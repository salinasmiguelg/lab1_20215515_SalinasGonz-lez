#lang racket
;Se define función git que recibirá los comandos
(define git (lambda (x)(lambda (zona) (lambda (acompanamiento)(cond ;se identifica y se ejecuta la función identificada
                           ((equal? x "pull") (pull zona))
                           ((equal? x "add") ((add zona)acompanamiento))
                           ((equal? x "commit") ((commit zona)acompanamiento))
                           ((equal? x "push") (push zona)))))))


(define append (lambda (listOne listTwo)
  (if (null? listOne)
       listTwo
      (cons (car listOne) (append (cdr listOne) listTwo))
      )
  ))




(define comprobarExistencia (lambda (x lista) (if(null? lista) #f (if (equal? x (car lista)) #t (comprobarExistencia x (cdr lista))))))


(define comprobarAdd (lambda (repositorio lista) (if (null?  lista) #t (if (comprobarExistencia (car lista) repositorio) (comprobarAdd repositorio (cdr lista) ) #f) )))

(define generadorCommit (lambda (comentario) (lambda (nombre) (list nombre comentario))))

(define quitarCommit (lambda (lista) (list-ref lista 0)) )
;Se define la función add
(define add (lambda (zona) (lambda (archivos) (if(list? archivos) (if(comprobarAdd (list-ref zona 0) archivos) (list (list-ref zona 0) (append (list-ref zona 1) archivos) (list-ref zona 2) (list-ref zona 3) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "add " (date) ) ")" " ") ) zona) zona) )))

;Se define la función commit
(define commit (lambda (zona) (lambda (comentario) (if (string? comentario) (list (list-ref zona 0) null (map (generadorCommit comentario) (list-ref zona 1)) (list-ref zona 3) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "commit " (date) ) ")" " ") )  zona) )))
;Se define la función push
(define push (lambda (zona) (list (list-ref zona 0) (list-ref zona 1) (list-ref zona 2) (append (list-ref zona 2) (list-ref zona 3)) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "push " (date) ) ")" " ") ) ))
;Se define la función pull
(define pull (lambda (zona) (list (append (list-ref zona 0) (map quitarCommit (list-ref zona 3 ))) (list-ref zona 1) (list-ref zona 2) (list-ref zona 3) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "pull " (date) ) ")" " ") ) ))


;El TDA de workSpace se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de index se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de localRepository se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de remoteRepository se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.



;Se define generador de ID
(define generadorID (lambda (nombreArchivo)(list nombreArchivo (random 100))))

(require racket/date)
(define date (lambda () (date->string (current-date) second)))

(define obtenerDatos (lambda (datos) (if(string? datos) (list datos (date)) null)))

(define datos? (lambda x (if(list? x) (if(and (string?(car(cdr x))) (string?(car x) (null?(cdr (cdr x))))) #t #f) #f )     ))


(define extraer (lambda (lista aux) (if (null? lista) "\n" (if (null? (cdr lista)) (string-append aux (car lista) "\n") (extraer (cdr lista) (string-append aux (car lista) " ")) ) )))
(define extraerListaDeLista (lambda (lista aux) (if (null? lista) "\n" (if (null? (cdr lista)) (string-append aux "(" (list-ref (car lista) 0) " " (list-ref (car lista) 1) ")" "\n" ) (extraerListaDeLista (cdr lista) (string-append aux "(" (list-ref (car lista) 0) " " (list-ref (car lista) 1) ")" " ")) ) )))

(define zonas->string (lambda (zona) (string-append "Nombre del repositorio: " (car(list-ref zona 4)) " "  (car(cdr(list-ref zona 4))) "\nWorkSpace: " (extraer (list-ref zona 0) "") "Index: " (extraer (list-ref zona 1) "") "LocalRepository: " (extraerListaDeLista (list-ref zona 2) "") "RemoteRepository: " (extraerListaDeLista (list-ref zona 3) "") "Historial de comandos: " (list-ref zona 5) )))


;TDA Zonas : Es una lista de TDA's donde cada uno de los argumentos representa cada una de las zonas donde se traspasa la información
(define zonas (lambda (workSpace index localRepository remoteRepository datos comando) (if(and(list? workSpace);TDA workSpace:  Consta de una lista que contiene los nombres de los archivos presentes en el directorio local del PC y sus id respectivas
                                                                                (list? index); TDA index: Consta de una lista
                                                                                (list? localRepository); TDA Local Repository : Consta de una lista que contiene id - fecha y hora - nombre de realizador - comentario - listado de archivos del commit - id version anterior
                                                                                (list? remoteRepository);TDA workSpace :
                                                                                (datos? datos)
                                                                                (list? comando)
                                                                                )(list workSpace index localRepository remoteRepository datos comando);retorno si es que todos son listas
                                                                                 null);retorno si uno de los argumentos no es lista
                                                                                ))


(define z1 (list (list "n1" "n2" "n3") null null null (obtenerDatos "repo Miguel") ""))
;(push((commit((add z1)(list "n1" "n2" "n3")) )"skere"))