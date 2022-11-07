      ******************************************************************
      * Author: Diego
      * Date:2022-02
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEAZAR0.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.


         01 WSC-INICIO             PIC  X(15)   VALUE 'PROCESO '.
         01 WSC-PROCESO            PIC  X(15)   VALUE ' DE NRO ' .
         01 WSC-FIN                PIC  X(15)   VALUE 'INCOGNITO' .
         01 WSC-PROGRAMA           PIC  X(08)   VALUE 'EJEAZAR0' .
         01 MASCARA                PIC  XXXX    VALUE 'xxxx'.
         01 EJEMPLO-CAMPO-N        PIC  99       VALUE 0.
         01  HORARIO               PIC 9(8).
         01  HORA REDEFINES HORARIO.
              02  HH               PIC 99.
              02  HM               PIC 99.
              02  HS               PIC 99.
               02  HX              PIC 99.
         01  NUMERO                PIC 9(6).


        01 CAMPO-AZAR.
           05 CAMPO-01             PIC 99.
           05 CAMPO-02             PIC 99.

       LINKAGE SECTION.
        01 AREA-COMUNICACION.
           05 CAMPO-INCOGNITO      PIC X(4).


       PROCEDURE DIVISION USING AREA-COMUNICACION.

       0000-CONTROL.

           PERFORM 1000-INICIO

           PERFORM 2000-PROCESO-CENTRAL

           PERFORM 3000-FINAL.



       1000-INICIO.

           DISPLAY WSC-INICIO  .


       2000-PROCESO-CENTRAL.
           DISPLAY WSC-PROCESO

      *      tomando la hora del sistema  obtenemos los 2 1eros digitos
      *         valor con calculo azaroso
           ACCEPT HORARIO FROM TIME.
           COMPUTE NUMERO = (HH * HX) / HM + HS.


           MOVE NUMERO         TO CAMPO-01

      *      tomando la hora del sistema  obtenemos los 2 sig digitos
      *         valor con calculo azaroso

           ACCEPT HORARIO FROM TIME.
           COMPUTE HH =  HH + HX
           COMPUTE HX = HX + HM
           COMPUTE HS = HS + HH
           COMPUTE NUMERO = (HH * HX) / HM + HS.


           MOVE NUMERO         TO CAMPO-02

           DISPLAY 'DEVUELVE NRO ' MASCARA
           MOVE    CAMPO-AZAR  TO CAMPO-INCOGNITO .

       3000-FINAL.
            DISPLAY WSC-FIN WSC-PROGRAMA.

           GOBACK.

