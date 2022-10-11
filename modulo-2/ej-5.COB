      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE005.
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
       01  WST-MATRIZ.
           05 WST-FILAS        OCCURS 3 TIMES INDEXED BY I.
               10 WST-COLS     OCCURS 3 TIMES INDEXED BY J.
                   15 WST-COL  PIC 9(06).

       77  WSV-ENTRADA         PIC 9(06).

       01  WSV-INDICE.
           05 WSV-INDICE-1     PIC 9(01).
           05 WSV-INDICE-2     PIC 9(01).
           05 WSV-INDICE-3     PIC 9(01).

       01  WSC-DIMENSIONES.
           05  WSC-DIM-FILA    PIC 9(01) VALUE 3.
           05  WSC-DIM-COL     PIC 9(01) VALUE 3.

       77  WSV-TOTAL-DIAGONAL  PIC 9(09).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM MOSTRAR-MATRIZ
           PERFORM SUMAR-DIAGONAL
           PERFORM MOSTRAR-SUMA-DIAGONAL
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-FILA
               PERFORM VARYING J FROM 1 BY 1 UNTIL 
               J > WSC-DIM-COL
                   DISPLAY "INGRESE DATO PARA LA FILA " I 
                   " Y COLUMNA " J
                   ACCEPT WSV-ENTRADA
                   MOVE WSV-ENTRADA TO WST-COL(I, J)
               END-PERFORM
           END-PERFORM. 
           
       MOSTRAR-MATRIZ.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-FILA
               DISPLAY WST-FILAS(I)
      *        DISPLAY WST-COL(I, WSV-INDICE-1) " " 
      *        WST-COL(I, WSV-INDICE-2) " " WST-COL(I, WSV-INDICE-3)  
           END-PERFORM.

       SUMAR-DIAGONAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-FILA
               PERFORM VARYING J FROM 1 BY 1 UNTIL 
               J > WSC-DIM-COL
                     IF I = J
                       COMPUTE WSV-TOTAL-DIAGONAL = WSV-TOTAL-DIAGONAL
                           + WST-COL(I,J)  
                     END-IF
               END-PERFORM
           END-PERFORM.

       MOSTRAR-SUMA-DIAGONAL.
           DISPLAY "TOTAL DIAGONAL: " WSV-TOTAL-DIAGONAL.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           