      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2005.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------
       DATA DIVISION.

      *-----------------------
       WORKING-STORAGE SECTION.
       01  WST-VECTOR-NOTAS.
           05 WST-ELEM         PIC 9(2) OCCURS 10 TIMES.

       01  WST-VECTOR-NOTAS-CONVERTIDO.
           05 WST-ELEM-C       PIC X(2)    OCCURS 10 TIMES.

       01  WSV-CONTADORES.
           05 I                PIC 9(02).

       01  WSC-DIM    PIC 9(02) VALUE 10.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM MOSTRAR-NOTAS
           PERFORM CONVERTIR-NOTAS
           PERFORM MOSTRAR-NOTAS-C
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           MOVE 10090807060504030201
            TO WST-VECTOR-NOTAS.
           
       MOSTRAR-NOTAS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM
               DISPLAY WST-ELEM(I) " " WITH NO ADVANCING
           END-PERFORM
           DISPLAY " ".

       MOSTRAR-NOTAS-C.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM
               DISPLAY WST-ELEM-C(I) " " WITH NO ADVANCING
           END-PERFORM
           DISPLAY " ".

       CONVERTIR-NOTAS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM
               PERFORM CONVERTIR-NOTA
           END-PERFORM. 
          

       CONVERTIR-NOTA.
           EVALUATE TRUE
               WHEN WST-ELEM(I) = 10
                   MOVE "A" TO WST-ELEM-C(I)
               WHEN WST-ELEM(I) = 9 OR WST-ELEM(I) = 8 
                   MOVE "B" TO WST-ELEM-C(I)
               WHEN WST-ELEM(I) = 7 OR WST-ELEM(I) = 6 OR 
               WST-ELEM(I) = 5 OR WST-ELEM(I) = 4
                   MOVE "C" TO WST-ELEM-C(I)
               WHEN WST-ELEM(I) = 3
                   MOVE "D" TO WST-ELEM-C(I)
               WHEN WST-ELEM(I) = 2
                   MOVE "E" TO WST-ELEM-C(I)
               WHEN WST-ELEM(I) = 1
                   MOVE "F" TO WST-ELEM-C(I)
           END-EVALUATE.
      
       FIN-DEL-PROGRAMA.
           STOP RUN.
           