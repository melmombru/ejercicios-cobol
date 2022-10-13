      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio 3x3
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES20043.
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
           05 WST-FILAS        OCCURS 3 TIMES ASCENDING KEY IS WST-COL.
               10 WST-COLS     OCCURS 3 TIMES ASCENDING KEY IS WST-COL.
                   15 WST-COL  PIC 9(1).
       
       01  WST-VECTOR-ESPIRAL.
           05 WST-ELEM         PIC 9(1) OCCURS 9 TIMES.
       
       01  WST-MATRIZ-ESPIRAL.          
           05 WST-FILAS-ESPIRAL        OCCURS 3 TIMES.
               10 WST-COLS-ESPIRAL     OCCURS 3 TIMES.
                   15 WST-COL-ESPIRAL  PIC 9(1).

      
       
       77  WSV-ENTRADA         PIC 9(06).

       01  WSV-CONTADORES.
           05 I                PIC 9(01).
           05 J                PIC 9(01).
           05 K                PIC 9(01) VALUE 1.

       01  WSC-DIMENSIONES.
           05  WSC-DIM-FILA    PIC 9(01) VALUE 3.
           05  WSC-DIM-COL     PIC 9(01) VALUE 3.

       01  WSV-ITERACION.
           05  WSV-FILA        PIC 9(01) VALUE 1.
           05  WSV-COL         PIC 9(01) VALUE 3.
           05  WSV-LEFT        PIC 9(01) VALUE 1.
           05  WSV-RIGHT       PIC 9(01) VALUE 3.

       77  WSV-AUX  PIC 9(01) VALUE 1.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM MOSTRAR-MATRIZ
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
      *    MOVE 987654321 TO WST-MATRIZ.
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
           PERFORM ORDENAR-ASCENDENTE VARYING I FROM 1 BY 1 
           UNTIL I > WSC-DIM-FILA 
           PERFORM REALIZAR-ESPIRAL.

       ORDENAR-ASCENDENTE.
           SORT WST-FILAS ASCENDING WST-COL 
           SORT WST-COLS(I) ASCENDING WST-COL.

       REALIZAR-ESPIRAL.
           PERFORM HACER-MATRIZ-ESPIRAL
           PERFORM MOSTRAR-ESPIRAL.           

       HACER-MATRIZ-ESPIRAL.
      *        WHEN 1
                   PERFORM VARYING I FROM WSV-LEFT BY 1 
                   UNTIL I > WSV-RIGHT
                       MOVE WST-COL(WSV-FILA,I) TO WST-ELEM(K)
                       COMPUTE K = K + 1
                   END-PERFORM
                   COMPUTE WSV-FILA = WSV-FILA + 1
                   SET WSV-AUX TO 2
      *        WHEN 2
                   PERFORM VARYING I FROM WSV-FILA BY 1 
                   UNTIL I > WSV-COL
                       MOVE WST-COL(WSV-RIGHT,I) TO WST-ELEM(K)
                       COMPUTE K = K + 1
                   END-PERFORM
                   COMPUTE WSV-RIGHT = WSV-RIGHT - 1
                   SET WSV-AUX TO 3
      *        WHEN 3
                   MOVE WSV-RIGHT TO I
                   PERFORM UNTIL I > WSV-COL
                       MOVE WST-COL(I,WSV-LEFT) TO WST-ELEM(K)
                       COMPUTE K = K + 1
                       COMPUTE I = I + 1
                   END-PERFORM
                   SET WSV-AUX TO 4
                   
                   MOVE WSV-COL TO I
                   PERFORM UNTIL I < WSV-FILA
                       MOVE WST-COL(WSV-FILA, I) TO WST-ELEM(K)
                       COMPUTE K = K + 1
                       COMPUTE I = I - 1
                   END-PERFORM
                   COMPUTE WSV-LEFT = WSV-LEFT - 1
                   SET WSV-AUX TO 1
           DISPLAY WST-VECTOR-ESPIRAL
           MOVE WST-VECTOR-ESPIRAL TO WST-MATRIZ-ESPIRAL.
       
       MOSTRAR-ESPIRAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-FILA
               PERFORM VARYING J FROM 1 BY 1 UNTIL 
               J > WSC-DIM-COL
                   DISPLAY WST-COL-ESPIRAL(I, J) " " 
                   WITH NO ADVANCING 
               END-PERFORM
               DISPLAY " "
           END-PERFORM.
           
       FIN-DEL-PROGRAMA.
           STOP RUN.
           