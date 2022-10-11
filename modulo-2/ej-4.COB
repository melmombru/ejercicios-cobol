      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE004.
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
       01  WSV-REG80           PIC X(80) VALUE "HOY ES MI CUMPLE;ANIOS".

       01  WST-REG80 REDEFINES WSV-REG80.
           05 WST-CARACTER     PIC X(1) OCCURS 80.

       77  WSV-NUM             PIC 9(2).

       77  WSV-HASTA           PIC 9(2).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM MUESTRA-FRASE
           PERFORM FIN-DEL-PROGRAMA.
           
       MUESTRA-FRASE.
           PERFORM VARYING WSV-NUM FROM 1 BY 1 UNTIL 
               WST-CARACTER(WSV-NUM) = ";"
               MOVE WSV-NUM TO WSV-HASTA  
               
           END-PERFORM
           DISPLAY WSV-REG80(1:WSV-HASTA).

       FIN-DEL-PROGRAMA.
           STOP RUN.
           