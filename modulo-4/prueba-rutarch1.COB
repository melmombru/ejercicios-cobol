      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: prueba rutinas ESCRIBIR ARCHIVO INDEX

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
       01  WSC-RUTINA                  PIC X(8) VALUE "RUTARCH".
       
       COPY REG-ARCHIVO.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-PROBAR-RUTINA
           PERFORM 0002-MOSTRAR-RETORNO
           PERFORM FIN-DEL-PROGRAMA.
           
       0001-PROBAR-RUTINA.
           MOVE "OOU" TO WSS-OPCION
           CALL WSC-RUTINA USING REGISTRO-ARCHIVO
           IF WSS-ST-OK
               MOVE "WRI" TO WSS-OPCION
               MOVE 01 TO REG-CLAVE-E
               MOVE "PEDRO" TO REG-NOMBRE-E
               MOVE 19990208 TO REG-FECHA-E
               CALL WSC-RUTINA USING REGISTRO-ARCHIVO
               IF WSS-ST-OK
                   MOVE "CLO" TO WSS-OPCION
                   CALL WSC-RUTINA USING REGISTRO-ARCHIVO
                   IF WSS-ST-OK
                       DISPLAY "FIN OK"
                   ELSE
                       PERFORM 0002-MOSTRAR-RETORNO
                   END-IF
               ELSE
                   PERFORM 0002-MOSTRAR-RETORNO
               END-IF
           ELSE
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.
       
       0002-MOSTRAR-RETORNO.
           DISPLAY WSV-ST-TEXTO
           DISPLAY WSV-ST-RETORNO.

       FIN-DEL-PROGRAMA.
           STOP RUN.
