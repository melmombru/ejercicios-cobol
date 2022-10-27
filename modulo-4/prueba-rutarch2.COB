      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: prueba rutinas LEER ARCHIVO INDEX

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PRUE-RUTINA2.
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
       01  WSC-RUTINA                  PIC X(8) VALUE "RUTARCH".
       
       COPY REG-ARCHIVO.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-PROBAR-RUTINA
           PERFORM FIN-DEL-PROGRAMA.
           
       0001-PROBAR-RUTINA.
           MOVE "OIN" TO WSS-OPCION
           CALL WSC-RUTINA USING REGISTRO-ARCHIVO
           IF WSS-ST-OK
               MOVE "REA" TO WSS-OPCION
               MOVE 01 TO REG-CLAVE-E
               CALL WSC-RUTINA USING REGISTRO-ARCHIVO
               IF WSS-ST-OK
                   DISPLAY "REGISTRO LEIDO " REG-ARCH-R
                   MOVE "CLO" TO WSS-OPCION
                   CALL WSC-RUTINA USING REGISTRO-ARCHIVO
                   IF WSS-ST-OK
                       DISPLAY "FIN OK"
                   ELSE
                       DISPLAY "ERROR CLOSE"
                       PERFORM 0002-MOSTRAR-RETORNO
                   END-IF
               ELSE
                   DISPLAY "ERROR READ"
                   PERFORM 0002-MOSTRAR-RETORNO
               END-IF
           ELSE
               DISPLAY "ERROR OPEN"
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.
       
       0002-MOSTRAR-RETORNO.
           DISPLAY WSV-ST-TEXTO
           DISPLAY WSV-ST-RETORNO.

       FIN-DEL-PROGRAMA.
           STOP RUN.
