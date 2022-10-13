      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2002.
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
       01  WST-VECTOR.
           05 WST-ELEM         PIC 9(05) OCCURS 20 TIMES INDEXED BY I.

       01  WSC-DIM-VECTOR      PIC 9(02) VALUE 20.

       01  WSV-FIBONACCI.
           05 WSV-ANTERIOR     PIC 9(05) VALUE 1.
           05 WSV-ANTERIOR-2   PIC 9(05) VALUE 1.
           05 WSV-ACTUAL       PIC 9(05).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM SERIE-FIBONACCI
           PERFORM MOSTRAR-VECTOR
           PERFORM FIN-DEL-PROGRAMA.

       SERIE-FIBONACCI.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-VECTOR
               IF I = 1 OR I = 2
                   MOVE WSV-ANTERIOR TO WST-ELEM(I)
               ELSE
                   COMPUTE WSV-ACTUAL = WSV-ANTERIOR + 
                           WSV-ANTERIOR-2
                   MOVE WSV-ANTERIOR TO WSV-ANTERIOR-2
                   MOVE WSV-ACTUAL TO WSV-ANTERIOR
                   MOVE WSV-ACTUAL TO WST-ELEM(I)
               END-IF
           END-PERFORM. 
           
       MOSTRAR-VECTOR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-VECTOR
               DISPLAY WST-ELEM(I)
           END-PERFORM.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           