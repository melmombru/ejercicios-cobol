      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE009A2.
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
       01  WSV-MESES.
           05 FILLER       PIC X(10) VALUE 'ENERO     '.
           05 FILLER       PIC X(10) VALUE 'FEBRERO   '.
           05 FILLER       PIC X(10) VALUE 'MARZO     '.
           05 FILLER       PIC X(10) VALUE 'ABRIL     '.
           05 FILLER       PIC X(10) VALUE 'MAYO      '.
           05 FILLER       PIC X(10) VALUE 'JUNIO     '.
           05 FILLER       PIC X(10) VALUE 'JULIO     '.
           05 FILLER       PIC X(10) VALUE 'AGOSTO    '.
           05 FILLER       PIC X(10) VALUE 'SEPTIEMBRE'.
           05 FILLER       PIC X(10) VALUE 'OCTUBRE   '.
           05 FILLER       PIC X(10) VALUE 'NOVIEMBRE '.
           05 FILLER       PIC X(10) VALUE 'DICIEMBRE '.

       01  WST-MESES REDEFINES WSV-MESES.
           05 WST-MES      PIC X(10) OCCURS 12.

       77  WSV-NRO-MES     PIC 9(2).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-MES
           PERFORM MUESTRA-MESES
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-MES.
           DISPLAY 'INGRESE UN NUMERO DEL 1 AL 12:'
           ACCEPT WSV-NRO-MES.
          
       MUESTRA-MESES.
           DISPLAY WST-MES(WSV-NRO-MES).

       FIN-DEL-PROGRAMA.
           STOP RUN.
           