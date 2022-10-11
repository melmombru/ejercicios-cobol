      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE009A3.
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

       77  WSV-NOMBRE-MES  PIC X(10).

       77  WSV-NUM         PIC 9(2).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-MES
           PERFORM MUESTRA-MESES
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-MES.
           DISPLAY 'INGRESE EL NOMBRE DE UN MES:'
           ACCEPT WSV-NOMBRE-MES.
          
       MUESTRA-MESES.
      *VARIANTE
      *PERFORM VARYING WSV-NUM FROM 1 BY 1 UNTIL 
      *    WSV-NOMBRE-MES = WST-MES(WSV-NUM)
           PERFORM VARYING WSV-NUM FROM 1 BY 1 UNTIL WSV-NUM > 12
               IF WSV-NOMBRE-MES = WST-MES(WSV-NUM)
                   DISPLAY WSV-NUM
               END-IF
           END-PERFORM.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           