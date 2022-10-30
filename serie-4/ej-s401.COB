      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: ej importes totalizay y lo muestra

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES4011.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

      *-----------------------
       WORKING-STORAGE SECTION.
       01  WST-IMPORTES.
           05 WST-IMPORTE      PIC 9(04)V9(02) OCCURS 6 TIMES 
                               INDEXED BY I.
               
       01 WSV-PAGO-TOTAL       PIC 9(06)V9(02).

       01 WSV-SEGUIR           PIC X(01).

       01 WSV-IMPORTE.
           05 WSV-TITULO       PIC X(50) VALUE ALL "*".
           05 WSV-PAGO         PIC X(33) VALUE 
                               "PAGOS TOTALES ULTIMO SEMESTRE ".
           05 WSV-FIN          PIC X(50) VALUE ALL "*".

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM INGRESO-DE-DATOS
           PERFORM CALCULAR-IMPORTE
           PERFORM INFORMAR-PAGO
           PERFORM FIN-DEL-PROGRAMA.
           
       INGRESO-DE-DATOS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               DISPLAY "INGRESAR IMPORTE"
               ACCEPT WST-IMPORTE(I)
           END-PERFORM.

       CALCULAR-IMPORTE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               ADD WST-IMPORTE(I) TO WSV-PAGO-TOTAL
           END-PERFORM.

       INFORMAR-PAGO.
           DISPLAY WSV-TITULO
           DISPLAY WSV-PAGO WITH NO ADVANCING
           DISPLAY WSV-PAGO-TOTAL
           DISPLAY WSV-FIN.

       FIN-DEL-PROGRAMA.
           STOP RUN.
