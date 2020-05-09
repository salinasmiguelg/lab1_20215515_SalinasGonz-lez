#lang racket
;TDA Zonas : Es una lista de TDA's donde cada uno de los argumentos representa cada una de las zonas donde se traspasa la información
(define zonas (lambda (workSpace index localRepository remoteRepository) (if(and(list? workSpace);
                                                                                (list? index);
                                                                                (list? localRepository); TDA Local Repository : Una lista que contiene id - fecha y hora - nombre de realizador - comentario - listado de archivos del commit - id version anterior
                                                                                (list? remoteRepository);
                                                                                )(list workSpace index localRepository remoteRepository);retorno si es que todos son listas
                                                                                 null);retorno si uno de los argumentos no es lista
                                                                                ))
