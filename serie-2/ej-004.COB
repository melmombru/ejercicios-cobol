      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2004.
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
           05 WST-FILAS        OCCURS 5 TIMES ASCENDING KEY 
                               IS WST-VALOR.
               10 WST-COLS     OCCURS 5 TIMES ASCENDING KEY 
                               IS WST-VALOR.
                   15 WST-VALOR  PIC 9(2).
       
       01  WST-VECTOR.
           05 WST-ELEM-N       PIC 9(2) OCCURS 25 TIMES.

       01  WST-VECTOR-ESPIRAL.
           05 WST-ELEM         PIC 9(2) OCCURS 25 TIMES.
       
       01  WST-MATRIZ-ESPIRAL.          
           05 WST-FILAS-ESPIRAL        OCCURS 5 TIMES.
               10 WST-COLS-ESPIRAL     OCCURS 5 TIMES.
                   15 WST-VALOR-ESPIRAL  PIC 9(2).

      
       
       77  WSV-ENTRADA         PIC 9(06).

       01  WSV-CONTADORES.
           05 I                PIC 9(02).
           05 J                PIC 9(02).
           05 K                PIC 9(03) VALUE 1.

       01  WSC-DIMENSIONES.
           05  WSC-DIM-FILA    PIC 9(01) VALUE 5.
           05  WSC-DIM-COL     PIC 9(01) VALUE 5.

       01  WSV-ITERACION.
           05  WSV-TOP         PIC 9(01) VALUE 1.
           05  WSV-BOTTOM      PIC 9(01) VALUE 5.
           05  WSV-LEFT        PIC 9(01) VALUE 1.
           05  WSV-RIGHT       PIC 9(01) VALUE 5.

       01  WSS-POS             PIC X(10).
           88 WSS-POS-TOP                VALUE "TOP".
           88 WSS-POS-BOTTOM             VALUE "BOTTOM".
           88 WSS-POS-LEFT               VALUE "LEFT".
           88 WSS-POS-RIGHT              VALUE "RIGHT". 
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM MOSTRAR-MATRIZ
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
      *    MOVE 25242322212019181716151413121110090807060504030201
      *     TO WST-MATRIZ.
           PERFORM VARYING I FROM 1 BY 1 UNTIL 
           I > (WSC-DIM-FILA * WSC-DIM-COL)
                   DISPLAY "INGRESE DATO NRO " I 
                   ACCEPT WSV-ENTRADA
                   MOVE WSV-ENTRADA TO WST-ELEM-N(I)
           END-PERFORM. 
           
       MOSTRAR-MATRIZ.
           PERFORM ORDENAR-ASCENDENTE 
           PERFORM REALIZAR-ESPIRAL.

       ORDENAR-ASCENDENTE.
           SORT WST-ELEM-N ASCENDING. 

       REALIZAR-ESPIRAL.
           PERFORM GENERAR-ESPIRAL
           PERFORM MOSTRAR-ESPIRAL.           

       GENERAR-ESPIRAL.
           DISPLAY WST-VECTOR
           PERFORM UNTIL WSV-TOP > WSV-BOTTOM OR WSV-LEFT > WSV-RIGHT
      *        TOP
               PERFORM ESPIRAL-TOP
      *        RIGHT
               PERFORM ESPIRAL-RIGHT
      *        BOTTOM
               PERFORM ESPIRAL-BOTTOM
      *        LEFT
               PERFORM ESPIRAL-LEFT
           END-PERFORM.
      
       ESPIRAL-TOP.
           PERFORM VARYING I FROM WSV-LEFT BY 1 
           UNTIL I > WSV-RIGHT
               DISPLAY WST-ELEM-N(K)
               MOVE WST-ELEM-N(K) TO 
               WST-VALOR-ESPIRAL(WSV-TOP, I)
               ADD 1 TO K
           END-PERFORM
           COMPUTE WSV-TOP = WSV-TOP + 1.

       ESPIRAL-BOTTOM.
           MOVE WSV-RIGHT TO I
           PERFORM UNTIL I < WSV-LEFT
               MOVE WST-ELEM-N(K) TO 
               WST-VALOR-ESPIRAL(WSV-BOTTOM, I)
               ADD 1 TO K
               COMPUTE I = I - 1
           END-PERFORM
           COMPUTE WSV-BOTTOM = WSV-BOTTOM - 1.

       ESPIRAL-RIGHT.
           PERFORM VARYING I FROM WSV-TOP UNTIL I > WSV-BOTTOM
               MOVE WST-ELEM-N(K) TO 
               WST-VALOR-ESPIRAL(I, WSV-RIGHT)
               ADD 1 TO K
           END-PERFORM
           COMPUTE WSV-RIGHT = WSV-RIGHT - 1.

       ESPIRAL-LEFT.
           MOVE WSV-BOTTOM TO I
           PERFORM UNTIL I < WSV-TOP
               MOVE WST-ELEM-N(K) TO 
               WST-VALOR-ESPIRAL(I, WSV-LEFT)
               ADD 1 TO K
               COMPUTE I = I - 1
           END-PERFORM
           COMPUTE WSV-LEFT = WSV-LEFT + 1.
       
       MOSTRAR-ESPIRAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM-FILA
               PERFORM VARYING J FROM 1 BY 1 UNTIL 
               J > WSC-DIM-COL
                   DISPLAY WST-VALOR-ESPIRAL(I, J) " " 
                   WITH NO ADVANCING 
               END-PERFORM
               DISPLAY " "
           END-PERFORM.
           
       FIN-DEL-PROGRAMA.
           STOP RUN.
           