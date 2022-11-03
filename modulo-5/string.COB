      ******************************************************************
      * Author: Melanie
      * Date: 2022 11
      * DESCRIPCION: USOS STRING
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. STRING.
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
       01  LAFECHA.
           02  FILLER  PIC X(7) VALUE "HOY ES ".
           02  LDIA    PIC Z9.
           02  FILLER  PIC X(4) VALUE " DE ".
           02  LMES    PIC X(10).
           02  FILLER  PIC X(4)  VALUE "DE ".
           02  LANIO   PIC 9999.
       01  FECHA.
           02  DIA     PIC 99 VALUE 22.
           02  MES     PIC 99 VALUE 06.
           02  ANIO    PIC 9999 VALUE 2001.
       01  CONSTRING   PIC X(40).
       01  TABLAMES.
           02  FILLER  PIC X(30) 
                           VALUE "ENERO     FEBRERO   MARZO    ".
           02  FILLER  PIC X(30) 
                           VALUE "ABRIL      MAYO       JUNIO  ".
           02  FILLER  PIC X(30) 
                           VALUE "JULIO      AGOSTO  SEPTIEMBRE".
           02  FILLER  PIC X(30) 
                           VALUE "OCTUBRE NOVIEMBREDICIEMBRE ".
       01  LATABLA REDEFINES TABLAMES.
           02  TMES    PIC X(10) OCCURS 12 TIMES.
       01  PUNTO       PIC 9(4) BINARY.
      *-----------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE DIA TO LDIA.
           MOVE TMES(MES) TO LMES.
           MOVE ANIO TO LANIO.
      *    STRING "HOY ES " DIA " DE " TMES(MES) " DE " ANIO
      *                        DELIMITED BY SIZE INTO CONSTRING
      *    DISPLAY "DIA " DIA
      *    DISPLAY "TMES(MES) " TMES(MES)
      *    DISPLAY "ANIO " ANIO
      *    DISPLAY "CONSTRING " CONSTRING
           MOVE 4 TO PUNTO.
           STRING "HOY ES " DIA " DE " DELIMITED BY SIZE
                 TMES (MES) DELIMITED BY " "
                 " DE " ANIO  DELIMITED BY SIZE INTO CONSTRING
                 WITH POINTER PUNTO.
           DISPLAY "CONSTRING2 " CONSTRING
           PERFORM FIN-DEL-PROGRAMA.

       FIN-DEL-PROGRAMA.
           STOP RUN.            
