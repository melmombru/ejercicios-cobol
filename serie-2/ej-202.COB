      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2102.
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
           05 FILLER       PIC 9(02) VALUE 31.
           05 FILLER       PIC X(10) VALUE 'FEBRERO   '.
           05 FILLER       PIC 9(02) VALUE 28.
           05 FILLER       PIC X(10) VALUE 'MARZO     '.
           05 FILLER       PIC 9(02) VALUE 31.
           05 FILLER       PIC X(10) VALUE 'ABRIL     '.
           05 FILLER       PIC 9(02) VALUE 30.
           05 FILLER       PIC X(10) VALUE 'MAYO      '.
           05 FILLER       PIC 9(02) VALUE 31.
           05 FILLER       PIC X(10) VALUE 'JUNIO     '.
           05 FILLER       PIC 9(02) VALUE 30.
           05 FILLER       PIC X(10) VALUE 'JULIO     '.
           05 FILLER       PIC 9(02) VALUE 31.
           05 FILLER       PIC X(10) VALUE 'AGOSTO    '.
           05 FILLER       PIC 9(02) VALUE 31.
           05 FILLER       PIC X(10) VALUE 'SEPTIEMBRE'.
           05 FILLER       PIC 9(02) VALUE 30.
           05 FILLER       PIC X(10) VALUE 'OCTUBRE   '.
           05 FILLER       PIC 9(02) VALUE 31.
           05 FILLER       PIC X(10) VALUE 'NOVIEMBRE '.
           05 FILLER       PIC 9(02) VALUE 30.
           05 FILLER       PIC X(10) VALUE 'DICIEMBRE '.
           05 FILLER       PIC 9(02) VALUE 31.

       01  WST-MESES REDEFINES WSV-MESES.
           05 WST-MES                     OCCURS 12 TIMES.
               10 WST-NOMBRE   PIC X(10).
               10 WST-DIAS     PIC 9(02).

       01  WSV-CONTADORES.
           05 I                PIC 9(02).

       01  WSC-DIM    PIC 9(02) VALUE 12.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM MOSTRAR-MESES
           PERFORM FIN-DEL-PROGRAMA.

       MOSTRAR-MESES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM
               DISPLAY "MES " WST-NOMBRE(I) "  CANTIDAD DE DIAS "
               WST-DIAS(I)
           END-PERFORM.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           