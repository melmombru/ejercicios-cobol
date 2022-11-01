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
       
       COPY REG-FECHA.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-PROBAR-RUTINA
           PERFORM 0002-MOSTRAR-RETORNO
           PERFORM FIN-DEL-PROGRAMA.
           
       0001-PROBAR-RUTINA.
           CALL WSC-RUTINA USING REGISTRO-FECHA.
       
       0002-MOSTRAR-RETORNO.
           IF WSS-RETORNO-OK
               DISPLAY "regreso de la rutina, la fecha es: "
               WSV-FECHA-TEXTUAL
           ELSE 
               DISPLAY "error al ingresar la fecha"
               DISPLAY WSV-TEXTO-STATUS
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
