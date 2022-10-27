      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Rutina acceso de archivo del ej 0013
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. RUTARCH.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA          ASSIGN TO DISK './DatosDePersonas.dat'
                               ORGANIZATION IS INDEXED
                               RECORD KEY IS REG-CLAVE
                               FILE STATUS IS WSS-FS-ENTRADA.
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA.
       01 REG-ENTRADA.
           05 REG-CLAVE            PIC X(2).
           05 REG-NOMBRE           PIC X(30).
           05 REG-FECHA            PIC X(10).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ENTRADA           PIC X(02).
           88 WSS-FS-ENTRADA-OK    VALUE '00'.
           88 WSS-FS-ENTRADA-EOF   VALUE '10'.

       01 WSV-CONT-ENTRADA     PIC S9(4) COMP.

       LINKAGE SECTION.
       COPY REG-ARCHIVO.
      *-----------------------
       PROCEDURE DIVISION USING REGISTRO-ARCHIVO.
       00000-CONTROL.
           EVALUATE TRUE
           WHEN WSS-OIN
               PERFORM 11000-ABRIR-ARCHIVO-INPUT
           WHEN WSS-OOU
               PERFORM 12000-ABRIR-ARCHIVO-OUTPUT
           WHEN WSS-OIO
               PERFORM 13000-ABRIR-ARCHIVO-INPUT-OUTPUT
           WHEN WSS-CLO
               PERFORM 30000-CERRAR-ARCHIVO
           WHEN WSS-REA
               PERFORM 21000-LEER-ARCHIVO
           WHEN WSS-WRI
               PERFORM 22000-ESCRIBIR-ARCHIVO
           WHEN OTHER
               SET WSS-ST-ERROR TO TRUE
               MOVE "ACCION SOLICITADA NO ENCONTRADA" 
               TO WSV-ST-TEXTO
           END-EVALUATE
           PERFORM FIN-DE-RUTINA.
           

       11000-ABRIR-ARCHIVO-INPUT.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN INPUT ENTRADA
           IF WSS-FS-ENTRADA-OK
               SET WSS-ST-OK TO TRUE
           ELSE
               MOVE WSS-FS-ENTRADA TO WSV-ST-RETORNO
               MOVE 'ERROR OPEN INPUT DEL ARCHIVO' 
               TO WSV-ST-TEXTO
           END-IF.  

       12000-ABRIR-ARCHIVO-OUTPUT.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN OUTPUT ENTRADA
           IF WSS-FS-ENTRADA-OK
               SET WSS-ST-OK TO TRUE
           ELSE
               MOVE WSS-FS-ENTRADA TO WSV-ST-RETORNO
               MOVE 'ERROR OPEN OUTPUT DEL ARCHIVO' 
               TO WSV-ST-TEXTO
           END-IF.
       
       13000-ABRIR-ARCHIVO-INPUT-OUTPUT.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN I-O ENTRADA
           IF WSS-FS-ENTRADA-OK
               SET WSS-ST-OK TO TRUE
           ELSE
               MOVE WSS-FS-ENTRADA TO WSV-ST-RETORNO
               MOVE 'ERROR OPEN I-O DEL ARCHIVO' 
               TO WSV-ST-TEXTO
           END-IF.

       21000-LEER-ARCHIVO.
           MOVE REG-CLAVE-E TO REG-CLAVE
           READ ENTRADA
           IF WSS-FS-ENTRADA-OK
               MOVE REG-ENTRADA TO REG-ARCH-R
               ADD 1 TO WSV-CONT-ENTRADA
               SET WSS-ST-OK TO TRUE
           ELSE 
               MOVE WSS-FS-ENTRADA TO WSV-ST-RETORNO
               MOVE 'ERROR EN EL READ DEL ARCHIVO' 
               TO WSV-ST-TEXTO
           END-IF.

       22000-ESCRIBIR-ARCHIVO.
           WRITE REG-ENTRADA FROM REG-ARCH-E.
           IF WSS-FS-ENTRADA-OK
               ADD 1 TO WSV-CONT-ENTRADA
               SET WSS-ST-OK TO TRUE
           ELSE 
               MOVE WSS-FS-ENTRADA TO WSV-ST-RETORNO
               MOVE 'ERROR EN EL WRITE DEL ARCHIVO' 
               TO WSV-ST-TEXTO
           END-IF.
       

       30000-CERRAR-ARCHIVO.
           CLOSE ENTRADA
           IF WSS-FS-ENTRADA-OK
               SET WSS-ST-OK TO TRUE
           ELSE
               MOVE WSS-FS-ENTRADA TO WSV-ST-RETORNO
               MOVE 'ERROR CLOSE DEL ARCHIVO' 
               TO WSV-ST-TEXTO
           END-IF.

       FIN-DE-RUTINA.
           GOBACK.
           

