      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE002M2.
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
           05 WST-VALOR        PIC 9(1) OCCURS 8.


       01  WSV-INDICES.         
           05 I                PIC 9(1).
           05 J                PIC 9(1).

       01  WSC-DIMENSION       PIC 9(02) VALUE 8.

       77  WSV-AUXILIAR        PIC 9(02).

       77  WSC-MAX             PIC 9(1) VALUE 8. 

      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM MOSTRAR-TABLA
           PERFORM ORDENAMIENTO-BURBUJA
           PERFORM MOSTRAR-TABLA
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           MOVE 84235176 TO WST-TABLA.
      *    PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIMENSION
      *        DISPLAY "INGRESE DATO PARA ELEMENTO " WSV-I
      *        ACCEPT WSV-ENTRADA
      *        MOVE WSV-ENTRADA TO WST-VALOR(WSV-I)
      *    END-PERFORM. 

      *MUESTRA POR PANTALLA LA TABLA      
       MOSTRAR-TABLA.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > WSC-DIMENSION
                   DISPLAY WST-VALOR(J) 
           END-PERFORM
           DISPLAY " ".

      *BUSCA EL MAXIMO DENTRO DE UNA MATRIZ
       ORDENAMIENTO-BURBUJA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-MAX
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WSC-MAX - I
                   IF WST-VALOR(J) > WST-VALOR(J + 1)
                       MOVE WST-VALOR(J) TO WSV-AUXILIAR
                       MOVE WST-VALOR(J + 1) TO WST-VALOR(J)
                       MOVE WSV-AUXILIAR TO WST-VALOR(J + 1)
                   END-IF
               END-PERFORM
               PERFORM MOSTRAR-TABLA
           END-PERFORM.
           
       FIN-DEL-PROGRAMA.
           STOP RUN.
         