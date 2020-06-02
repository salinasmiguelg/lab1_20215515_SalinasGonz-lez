#lang racket
;CONSTRUCTORES
;El TDA de workSpace se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de index se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de localRepository se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;El TDA de remoteRepository se define como una lista de strings, de cualquier largo. Por lo que no se presenta constructor de este TDA.
;TDA Zonas : Es una lista de TDA's donde cada uno de los argumentos representa cada una de las zonas donde se traspasa la información
(define zonas (lambda (workSpace index localRepository remoteRepository datos comando)  (list workSpace index localRepository remoteRepository datos comando)
                                                                                ))

;SELECTORES
;seleccionar WorkSpace
(define selWorkSpace (lambda (zona) (list-ref zona 0)))
;seleccionar Index
(define selIndex (lambda (zona) (list-ref zona 1)))
;seleccionar LocalRepository
(define selLocalRepository (lambda (zona) (list-ref zona 2)))
;seleccionar RemoteRepository
(define selRemoteRepository (lambda (zona) (list-ref zona 3)))
;seleccionar Datos
(define selDatos (lambda (zona) (list-ref zona 4)))
;seleccionar Historial de Comandos
(define selComandos (lambda (zona) (list-ref zona 5)))

;Pertenencia
(define zona? (lambda (zona) (if (and(list? (selWorkSpace zona));TDA workSpace:  Consta de una lista que contiene los nombres de los archivos presentes en el directorio local del PC y sus id respectivas
                                                                                (list? (selIndex zona)); TDA index: Consta de una lista
                                                                                (list? (selLocalRepository zona)); TDA Local Repository : Consta de una lista que contiene id - fecha y hora - nombre de realizador - comentario - listado de archivos del commit - id version anterior
                                                                                (list? (selRemoteRepository));TDA workSpace :
                                                                                (datos? (selDatos zona))
                                                                                (list? (selComandos zona))
                                                                                ) #t #f)))



;MODIFICADORES

;Se define
;Descripción:comprueba existencia
;Argumentos:una lista y un elemento
;Retorno:bool
;Recursividad:natural
(define comprobarExistencia (lambda
                                (x lista)
                                   (if(null? lista) #f (if (equal? x (car lista)) #t (comprobarExistencia x (cdr lista))))
                              ))

;Se define
;Descripción: comprueba la existencia de los elementos de la lista en el workspace
;Argumentos: dos listas 
;Retorno: bool
;Recursividad:natural
(define comprobarAdd (lambda
                         (repositorio lista)
                            (if (null?  lista) #t (if (comprobarExistencia (car lista) repositorio) (comprobarAdd repositorio (cdr lista) ) #f) )
                       ))
;Se define
;Descripción: genera commit
;Argumentos: un par de string
;Retorno: una lista de largo dos de strings
;Recursividad:no tiene
(define generadorCommit (lambda
                            (comentario)
                              (lambda (nombre) (list nombre comentario))
                          ))
;Se define
;Descripción: quita un commit
;Argumentos: una lista
;Retorno: un string
;Recursividad:no tiene
(define quitarCommit (lambda (lista) (list-ref lista 0)) )




;Se define función git que recibirá los comandos
;Descripción: englobal el resto de comandos de git
;Argumentos:un string o una lista y una zona
;Retorno:una zona
;Recursividad: no tiene
(define git (lambda (x)(lambda (zona) (lambda (acompanamiento)(cond ;se identifica y se ejecuta la función identificada
                           ((equal? x "pull") (pull zona))
                           ((equal? x "add") ((add zona)acompanamiento))
                           ((equal? x "commit") ((commit zona)acompanamiento))
                           ((equal? x "push") (push zona)))))))








;Se define la función add
;Descripción: Ejecuta la función add que copia lo de una lista en workspace al index
;Argumentos: Una zona y una lista de stringa
;Retorno: Una zona
;Recursividad: No tiene
(define add (lambda (zona) (lambda (archivos) (if(list? archivos) (if(comprobarAdd (list-ref zona 0) archivos) (list (list-ref zona 0) (append (list-ref zona 1) archivos) (list-ref zona 2) (list-ref zona 3) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "add " (date) ) ")" " ") ) zona) zona) )))

;Se define la función commit
;Descripción: genera un commit moviendo del index al local y agregandole un comentario
;Argumentos: una zona y un comentario
;Retorno: una zona
;Recursividad:no tiene
(define commit (lambda (zona) (lambda (comentario) (if (string? comentario) (list (list-ref zona 0) null (map (generadorCommit comentario) (list-ref zona 1)) (list-ref zona 3) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "commit " (date) ) ")" " ") )  zona) )))

;Se define la función push
;Descripción: copia lo del local y lo pega en el remote
;Argumentos: una zona
;Retorno: una zona
;Recursividad: no tiene
(define push (lambda (zona) (list (list-ref zona 0) (list-ref zona 1) (list-ref zona 2) (append (list-ref zona 2) (list-ref zona 3)) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "push " (date) ) ")" " ") ) ))

;Se define la función pull
;Descripción: trae lo de remote al workspace
;Argumentos: una zona
;Retorno: una zona
;Recursividad: no tiene
(define pull (lambda (zona) (list (append (list-ref zona 0) (map quitarCommit (list-ref zona 3 ))) (list-ref zona 1) (list-ref zona 2) (list-ref zona 3) (list-ref zona 4) (string-append (list-ref zona 5) "(" (string-append "pull " (date) ) ")" " ") ) ))






;se define date
;Descripción: extrae la hora del pc
;Argumentos:ninguno
;Retorno: un string con la fecha y hora a la que se ejecuto el comando
;Recursividad: no tiene
(require racket/date)
(define date (lambda () (date->string (current-date) second)))
;Constructor de datos
(define obtenerDatos (lambda (datos) (if(string? datos) (list datos (date)) null)))
;pertenencia de datos
(define datos? (lambda x (if(list? x) (if(and (string?(car(cdr x))) (string?(car x) (null?(cdr (cdr x))))) #t #f) #f )     ))

;
;se define extraer
;Descripción: extrae lo de una lista y lo ingresa en un string
;Argumentos: una lista y un auxiliar
;Retorno: un string con lo de la lista
;Recursividad: de cola
(define extraer (lambda (lista aux) (if (null? lista) "\n" (if (null? (cdr lista)) (string-append aux (car lista) "\n") (extraer (cdr lista) (string-append aux (car lista) " ")) ) )))
;se define extraer lista de lista
;Descripción: extrae lo de una lista y lo ingresa en un string
;Argumentos:una lista de lista
;Retorno: un string
;Recursividad: de cola.
(define extraerListaDeLista (lambda (lista aux) (if (null? lista) "\n" (if (null? (cdr lista)) (string-append aux "(" (list-ref (car lista) 0) " " (list-ref (car lista) 1) ")" "\n" ) (extraerListaDeLista (cdr lista) (string-append aux "(" (list-ref (car lista) 0) " " (list-ref (car lista) 1) ")" " ")) ) )))
;

;Se define zonas->string
;Descripción: muestra de manera más cómoda visiblemente los repositorios actuales
;Argumentos:una zona
;Retorno: un string 
;Recursividad:no tiene
(define zonas->string (lambda (zona) (string-append "Nombre del repositorio: " (car(list-ref zona 4)) " "  (car(cdr(list-ref zona 4))) "\nWorkSpace: " (extraer (list-ref zona 0) "") "Index: " (extraer (list-ref zona 1) "") "LocalRepository: " (extraerListaDeLista (list-ref zona 2) "") "RemoteRepository: " (extraerListaDeLista (list-ref zona 3) "") "Historial de comandos: " (list-ref zona 5) )))





(define zona_1 (list (list "arch.rkt" "arch2.rkt" "arch3.rkt") null null null (obtenerDatos "repositorio 1 Miguel Salinas") ""))
(push((commit((add zona_1)(list "arch.rkt" "arch2.rkt" "arch3.rkt")) )"mensaje commit"))


(define zona_2 (list (list "arch4.rkt") null null (list (list "arch.rkt" "mensaje") (list "arch2.rkt" "mensaje") (list "arch3.rkt" "mensaje")) (obtenerDatos "repositorio 2 Micky Salinas") ""))




(define zona_3 (list (list "arch.rkt" "arch2.rkt" "arch3.rkt" "arch4.rkt") (list "arch4.rkt") (list (list "arch.rkt" "mensaje")) (list (list "arch.rkt" "mensaje")) (obtenerDatos "repositorio 2 Mipanamiguel Salinas") ""))
