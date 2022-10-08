      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio M1-3
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE30M1.
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
       01  FECHA.
           05 DIA         PIC 9(2).
           05 MES         PIC 9(2).
           05 ANIO        PIC 9(4).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-FECHA
           PERFORM MUESTRA-FECHA
           PERFORM FIN-DEL-PROGRAMA.
           

       INGRESO-DE-FECHA.
           
           DISPLAY 'INGRESE LA FECHA (en formato ddmmaaaa):'
           ACCEPT FECHA.

       MUESTRA-FECHA.
           DISPLAY 'FECHA INGRESADA: ' FECHA.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           

