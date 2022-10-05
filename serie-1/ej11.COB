      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio 11
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE110S1.
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
       77 FECHA PIC X(10) VALUE '2006-04-18'.
      *-----------------------
       PROCEDURE DIVISION.
       PROCESO.
           DISPLAY 'ANIO: ' FECHA(1:4)
           DISPLAY 'MES:  '  FECHA(6:2)
           DISPLAY 'DIA:  '  FECHA(9:2).
