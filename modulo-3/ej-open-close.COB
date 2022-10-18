      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: 

      *Ejercicio 2     

      *Archiv  Factura  (Secuencial -Salida impresa)  

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE011D1.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA          ASSIGN TO DISK './CUENTAS001.DAT'
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WSS-FS-ENTRADA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA.
       01 REG-ENTRADA.
           05 LINEA-MATRIZ     PIC X(30).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ENTRADA           PIC X(02).
           88 WSS-FS-ENTRADA-OK    VALUE '00'.
           88 WSS-FS-ENTRADA-EOF   VALUE '10'.

       01 WSV-CONT-ENTRADA     PIC S9(4) COMP.
      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM 15000-LEER-Y-CARGAR UNTIL WSS-FS-ENTRADA-EOF
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM FIN-DEL-PROGRAMA.
           
       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN INPUT ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.

       15000-LEER-Y-CARGAR.
           READ ENTRADA
           IF WSS-FS-ENTRADA-OK
               DISPLAY LINEA-MATRIZ
               ADD 1 TO WSV-CONT-ENTRADA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.