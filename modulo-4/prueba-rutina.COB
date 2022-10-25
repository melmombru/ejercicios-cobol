      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: prueba rutinas 

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PRUE-RUTINA.
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
       01  WSC-RUTINA                  PIC X(8) VALUE "RUTFECHA".
       
       01  REGISTRO-LLAMADO.
           05 DATOS-RETORNO.
               10 WSV-FECHA-NUMERICA   PIC 9(08).
               10 WSV-FECHA-TEXTUAL    PIC X(20).
           05 STATUS-DATOS.
               10 WSV-STATUS-RETORNO   PIC 9(02) VALUE 00.
                   88 WSS-RETORNO-OK             VALUE 00.
                   88 WSS-RETORNO-ERROR          VALUE 11.
               10 WSV-TEXTO-STATUS     PIC X(20).

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-PROBAR-RUTINA
           PERFORM 0002-MOSTRAR-RETORNO
           PERFORM FIN-DEL-PROGRAMA.
           
       0001-PROBAR-RUTINA.
           CALL WSC-RUTINA USING REGISTRO-LLAMADO.
       
       0002-MOSTRAR-RETORNO.
           IF WSS-RETORNO-OK
               DISPLAY "regreso de la rutina, la fecha es: "
               WSV-FECHA-TEXTUAL
           ELSE 
               DISPLAY "error al ingresar la fecha"
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
