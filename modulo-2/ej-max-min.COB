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
       01  WST-TABLA.
           05 WST-VALOR      PIC 9(02) OCCURS 10.


       01  WSV-INDICES.         
           05 WSV-I                PIC 9(02).

       01  WSC-DIMENSION           PIC 9(02) VALUE 10.

       77  WSV-AUX                 PIC 9(05).
       77  WSV-ENTRADA             PIC 9(02).
       77  WSV-CONTADOR-1          PIC 9(02).
       77  WSV-CONTADOR-2          PIC 9(02).
       77  WSV-ACUMULADOR-1        PIC 9(02).
       77  WSV-ACUMULADOR-2        PIC 9(02).
       77  WSV-PROMEDIO            PIC 9(02).
       77  WSC-MAX                 PIC 9(02) VALUE 99. 

      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM PROCESO-MOSTRAR-MATRIZ
           PERFORM PROCESO-BUSCAR-MAXIMO
           PERFORM PROCESO-BUSCAR-MINIMO
           PERFORM PROCESO-PROMEDIO
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
      *    MOVE 090807060504030201 TO WST-MATRIZ.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIMENSION
               DISPLAY "INGRESE DATO PARA ELEMENTO " WSV-I
               ACCEPT WSV-ENTRADA
               MOVE WSV-ENTRADA TO WST-VALOR(WSV-I)
           END-PERFORM. 

      *MUESTRA POR PANTALLA LA MATRIZ      
       PROCESO-MOSTRAR-MATRIZ.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIMENSION
                   DISPLAY WST-VALOR(WSV-I) 
           END-PERFORM.

      *BUSCA EL MAXIMO DENTRO DE UNA MATRIZ
       PROCESO-BUSCAR-MAXIMO.
           MOVE ZEROES TO WSV-AUX
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIMENSION
               IF WST-VALOR(WSV-I) > WSV-AUX
                   MOVE WST-VALOR(WSV-I) TO WSV-AUX
               END-IF
           END-PERFORM
           DISPLAY "MAXIMO: " WSV-AUX.
       
      
      *BUSCA EL MINIMO DENTRO DE UNA MATRIZ
       PROCESO-BUSCAR-MINIMO.
           MOVE WSC-MAX TO WSV-AUX
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIMENSION
               IF WST-VALOR(WSV-I) < WSV-AUX
                   MOVE WST-VALOR(WSV-I) TO WSV-AUX
               END-IF
           END-PERFORM
           DISPLAY "MINIMO: " WSV-AUX.

       PROCESO-PROMEDIO.
           MOVE ZEROES TO WSV-ACUMULADOR-1
           MOVE ZEROES TO WSV-PROMEDIO
           MOVE ZEROES TO WSV-CONTADOR-1
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIMENSION
               IF WSV-I <= 5 
                   ADD WST-VALOR(WSV-I) TO WSV-ACUMULADOR-1
                   ADD 1 TO WSV-CONTADOR-1
               ELSE
                   ADD WST-VALOR(WSV-I) TO WSV-ACUMULADOR-2
                   ADD 1 TO WSV-CONTADOR-2
               END-IF
           END-PERFORM
           COMPUTE WSV-PROMEDIO = WSV-ACUMULADOR-1 / WSV-CONTADOR-1
           DISPLAY "PROMEDIO: " WSV-PROMEDIO
           COMPUTE WSV-PROMEDIO = WSV-ACUMULADOR-2 / WSV-CONTADOR-2
           DISPLAY "PROMEDIO: " WSV-PROMEDIO.
           
       FIN-DEL-PROGRAMA.
           STOP RUN.
           