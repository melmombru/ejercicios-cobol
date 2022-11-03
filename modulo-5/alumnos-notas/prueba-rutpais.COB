      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: prueba rutinas RUTPAIS

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PRUE-RUTINA1.
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
       01  WSC-RUTINA                  PIC X(8) VALUE "RUTPAIS".
       
       COPY REG-PAIS.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-PROBAR-RUTINA
           PERFORM FIN-DEL-PROGRAMA.
           
        0001-PROBAR-RUTINA.
           MOVE "A" TO WSS-OPCION-PAIS
           CALL WSC-RUTINA USING REGISTRO-PAIS
           IF WSS-PAIS-OK
               MOVE "B" TO WSS-OPCION-PAIS
               MOVE 054 TO REG-NRO-PAIS-E
               CALL WSC-RUTINA USING REGISTRO-PAIS
               IF WSS-PAIS-OK
                   DISPLAY "PAIS ENCONTRADO " REG-DESCRIP-PAIS-R
               ELSE
                   PERFORM 0002-MOSTRAR-RETORNO
               END-IF
           ELSE
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.
       
       0002-MOSTRAR-RETORNO.
           DISPLAY WSV-ST-TEXTO-PAIS
           DISPLAY WSV-ST-RETORNO-PAIS.

       FIN-DEL-PROGRAMA.
           STOP RUN.
