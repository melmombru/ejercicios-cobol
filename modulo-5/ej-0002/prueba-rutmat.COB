      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: prueba rutinas RUTMAT

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
       01  WSC-RUTINA                  PIC X(8) VALUE "RUTMAT".
       
       COPY REG-MAT.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-PROBAR-RUTINA
           PERFORM FIN-DEL-PROGRAMA.
           
        0001-PROBAR-RUTINA.
           MOVE "A" TO WSS-OPCION-MAT
           CALL WSC-RUTINA USING REGISTRO-MATERIA
           IF WSS-MAT-OK
               MOVE "B" TO WSS-OPCION-MAT
               MOVE 01 TO REG-NRO-MAT-E
               CALL WSC-RUTINA USING REGISTRO-MATERIA
               IF WSS-MAT-OK
                   DISPLAY "MATERIA ENCONTRADA " REG-DESCRIP-MAT-R
               ELSE
                   PERFORM 0002-MOSTRAR-RETORNO
               END-IF
           ELSE
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.
       
       0002-MOSTRAR-RETORNO.
           DISPLAY WSV-ST-TEXTO-MAT
           DISPLAY WSV-ST-RETORNO-MAT.

       FIN-DEL-PROGRAMA.
           STOP RUN.
