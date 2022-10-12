      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE0009A4.
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
       01 WSC-MES .
           05 FILLER PIC X(30) VALUE '00345-00500-00445-00090-00000-'.
           05 FILLER PIC X(30) VALUE '00360-00455-00440-00095-00000-'.
           05 FILLER PIC X(30) VALUE '00333-00521-00446-00094-00000-'.
           05 FILLER PIC X(30) VALUE '00300-00654-00443-00100-00000-'.
           05 FILLER PIC X(30) VALUE '00345-00590-00454-00089-00000-'.
           05 FILLER PIC X(30) VALUE '00380-00566-00490-00101-00000-'.
           05 FILLER PIC X(30) VALUE '00323-00600-00435-00092-00000-'.
           05 FILLER PIC X(30) VALUE '00299-00532-00390-00085-00000-'.
           05 FILLER PIC X(30) VALUE '00346-00534-00449-00090-00000-'.
           05 FILLER PIC X(30) VALUE '00321-00536-00545-00095-00000-'.
           05 FILLER PIC X(30) VALUE '00344-00569-00345-00093-00000-'.
           05 FILLER PIC X(30) VALUE '00380-00566-00390-00075-00000-'.
           05 FILLER PIC X(30) VALUE '00000-00000-00000-00000-00000-'.

       01 WST-MESES REDEFINES WSC-MES.
      *    X 1 A 13
           03 WST-MESES-DET        OCCURS 13.
      *        Y 1 A 5
               05 WST-GASTOS-DET   OCCURS 5. 
                   10 WST-GASTOS   PIC 9(5).
                   10 FILLER       PIC X.


       01  WSV-INDICES.         
           05 WSV-I                PIC 9(02).
           05 WSV-J                PIC 9(01).

       01  WSC-DIMENSIONES.
           05  WSC-DIM-FILA        PIC 9(02) VALUE 13.
           05  WSC-DIM-COL         PIC 9(01) VALUE 5.

       77  WSV-TOTAL-COL           PIC 9(05).
       77  WSV-TOTAL-FILA          PIC 9(05).

       01  WSV-TOTALES.
           05 WSV-TOTAL-COLUMNA-5  PIC 9(06).
           05 WSV-TOTAL-FILA-13    PIC 9(06).

      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM PROCESO-MOSTRAR-MATRIZ
           PERFORM PROCESO-SUMAR-COLUMNAS
           PERFORM PROCESO-SUMAR-FILAS
           PERFORM PROCESO-SUMAR-TOTALES
           PERFORM PROCESO-VALIDAR-SUMAS
           PERFORM FIN-DEL-PROGRAMA.

      *MUESTRA POR PANTALLA LA MATRIZ      
       PROCESO-MOSTRAR-MATRIZ.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIM-FILA
               PERFORM VARYING WSV-J FROM 1 BY 1 UNTIL 
               WSV-J > WSC-DIM-COL
                   DISPLAY WST-GASTOS(WSV-I, WSV-J) " " 
                   WITH NO ADVANCING 
               END-PERFORM
               DISPLAY " "
           END-PERFORM.

      *SUMA LAS COLUMNAS DEL 1 AL 4
       PROCESO-SUMAR-COLUMNAS.
           DISPLAY "-------------------------------"
           DISPLAY "        SUMAR COLUMNAS"
           DISPLAY "-------------------------------"

           PERFORM SUMAR-TOTAL-COLUMNA VARYING WSV-J FROM 1 BY 1 UNTIL 
           WSV-J > (WSC-DIM-COL - 1)
           PERFORM PROCESO-MOSTRAR-MATRIZ.
       
      *SUMA EL TOTAL DE LA COLUMNA WSV-J Y LO GUARDA EN LA POSICION
      *(WSC-DIM-FILA, WSV-J)
       SUMAR-TOTAL-COLUMNA.
           PERFORM SUMAR-COLUMNA
           MOVE WSV-TOTAL-COL TO WST-GASTOS(WSC-DIM-FILA, WSV-J)
           MOVE 0 TO WSV-TOTAL-COL.
       
      *SUMA EL TOTAL DE LA COLUMNA WSV-J  
       SUMAR-COLUMNA.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL 
               WSV-I > WSC-DIM-FILA
                   COMPUTE WSV-TOTAL-COL = WSV-TOTAL-COL 
                           + WST-GASTOS(WSV-I, WSV-J)
           END-PERFORM.

      *SUMA LAS FILAS DEL 1 AL 12
       PROCESO-SUMAR-FILAS.
           DISPLAY "-------------------------------"
           DISPLAY "        SUMAR FILAS"
           DISPLAY "-------------------------------"

           PERFORM SUMAR-TOTAL-FILA VARYING WSV-I FROM 1 BY 1 UNTIL 
           WSV-I > (WSC-DIM-FILA - 1)
           PERFORM PROCESO-MOSTRAR-MATRIZ.
       
      *SUMA EL TOTAL DE LA FILA WSV-I Y LO GUARDA EN LA POSICION
      *(WSV-I, WSC-DIM-COL)
       SUMAR-TOTAL-FILA.
           PERFORM SUMAR-FILA
           MOVE WSV-TOTAL-FILA TO WST-GASTOS(WSV-I, WSC-DIM-COL)
           MOVE 0 TO WSV-TOTAL-FILA.

      *SUMA EL TOTAL DE LA FILA WSV-I
       SUMAR-FILA.
           PERFORM VARYING WSV-J FROM 1 BY 1 UNTIL 
               WSV-J > WSC-DIM-COL
                   COMPUTE WSV-TOTAL-FILA = WSV-TOTAL-FILA 
                           + WST-GASTOS(WSV-I, WSV-J)
           END-PERFORM.
      
      *SUMA LOS TOTALES DE LA FILA 13 Y DE LA COLUMNA 15
       PROCESO-SUMAR-TOTALES.
           DISPLAY "-------------------------------"
           DISPLAY "        SUMAR TOTALES"
           DISPLAY "-------------------------------"
           PERFORM PROCESO-SUMAR-TOTAL-COLUMNA-5
           PERFORM PROCESO-SUMAR-TOTAL-FILA-13.

      *SUMA EL TOTAL DE LA COLUMNA 5 Y LO GUARDA EN LA VARIABLE
      *WSV-TOTAL-COLUMNA-5
       PROCESO-SUMAR-TOTAL-COLUMNA-5.
           MOVE WSC-DIM-COL TO WSV-J
           PERFORM SUMAR-COLUMNA
           MOVE WSV-TOTAL-COL TO WSV-TOTAL-COLUMNA-5
           DISPLAY "SUMA TOTAL COLUMNA 5: " WSV-TOTAL-COLUMNA-5.
       
      *SUMA EL TOTAL DE LA FILA 13 Y LO GUARDA EN LA VARIABLE
      *WSV-TOTAL-FILA-13
       PROCESO-SUMAR-TOTAL-FILA-13.
           MOVE WSC-DIM-FILA TO WSV-I
           PERFORM SUMAR-FILA
           MOVE WSV-TOTAL-FILA TO WSV-TOTAL-FILA-13
           DISPLAY "SUMA TOTAL FILA 13:   " WSV-TOTAL-FILA-13.

      *SI EL TOTAL DE LA FILA 13 Y EL DE LA COLUMNA 5 SON IGUALES,
      *LOS GUARDA EN LA ULTIMA POSICION DE LA MATRIZ
       PROCESO-VALIDAR-SUMAS.
           DISPLAY "-------------------------------"
           DISPLAY "        VALIDAR SUMAS          "
           DISPLAY "-------------------------------"
           IF WSV-TOTAL-COLUMNA-5 = WSV-TOTAL-FILA-13
               MOVE WSV-TOTAL-FILA-13 TO 
               WST-GASTOS(WSC-DIM-FILA, WSC-DIM-COL)
           END-IF
           PERFORM PROCESO-MOSTRAR-MATRIZ.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           