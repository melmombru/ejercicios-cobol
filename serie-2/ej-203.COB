      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2103.
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

       01  WSV-ENTRADA.
           05 WSV-MES          PIC 9(02).
           05 WSV-ANIO         PIC 9(04).

       01  WSV-CONTADORES.
           05 I                PIC 9(02).

       77  WSV-MODULO          PIC 9(3)V9(3).

       01  WSC-DIM    PIC 9(02) VALUE 12.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM MOSTRAR-CANTIDAD-DIAS
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           DISPLAY "INGRESE MES: "
           ACCEPT WSV-MES
           DISPLAY "INGRESE ANIO: "
           ACCEPT WSV-ANIO.
      
       MOSTRAR-CANTIDAD-DIAS.
           IF WSV-MES = 02
               PERFORM CALCULAR-ANIO-BISIESTO
           END-IF
           DISPLAY "MES " WST-NOMBRE(WSV-MES) "  CANTIDAD DE DIAS "
           WST-DIAS(WSV-MES).

       CALCULAR-ANIO-BISIESTO.
           COMPUTE WSV-MODULO = WSV-ANIO / 4
           IF WSV-MODULO(4:3) = 000
               COMPUTE WSV-MODULO = WSV-ANIO / 100
               IF (WSV-MODULO(4:3) NOT = 000)
                   ADD 1 TO WST-DIAS(WSV-MES)
               ELSE   
                   COMPUTE WSV-MODULO = WSV-ANIO / 400  
                   IF (WSV-MODULO(4:3) = 000)
                       ADD 1 TO WST-DIAS(WSV-MES)
                   END-IF
               END-IF
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           