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

       SELECT SALIDA          ASSIGN TO DISK './DatosDePersonas.dat'
                              ORGANIZATION IS INDEXED
                              RECORD KEY IS REG-CLAVE
                              ACCESS MODE RANDOM
                              FILE STATUS IS WSS-FS-SALIDA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD SALIDA.
       01 REG-SALIDA1.
           05 REG-CLAVE            PIC X(2).
           05 REG-NOMBRE           PIC X(30).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-SALIDA            PIC X(02).
           88 WSS-FS-SALIDA-OK     VALUE '00'.
           88 WSS-FS-SALIDA-EOF    VALUE '10'.

       01 WSV-CONT-SALIDA          PIC S9(4) COMP.

       01 WSV-ENTRADA.
           05 WSV-CLAVE            PIC X(02).
           05 WSV-NOMBRE           PIC X(30).
       
       01 WSS-ENCONTRADO           PIC X(02).
           88 WSS-SI               VALUE 'SI'.
           88 WSS-NO               VALUE 'NO'.
      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM INGRESO-DE-DATOS
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.
           
       INGRESO-DE-DATOS.
           INITIALIZE REG-SALIDA1
           DISPLAY "INGRESAR CLAVE: "
           ACCEPT REG-CLAVE
           PERFORM 15000-BUSCAR-PERSONA.
       
       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-SALIDA
           OPEN INPUT SALIDA
           IF NOT WSS-FS-SALIDA-OK
               DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
               PERFORM FIN-DEL-PROGRAMA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE SALIDA
           IF NOT WSS-FS-SALIDA-OK
               DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.

       15000-BUSCAR-PERSONA.
           MOVE "SI" TO WSS-ENCONTRADO
           READ SALIDA NOT INVALID KEY MOVE "NO" TO WSS-ENCONTRADO
           IF WSS-FS-SALIDA-OK
               DISPLAY REG-SALIDA1
               ADD 1 TO WSV-CONT-SALIDA
           END-IF
           IF WSS-SI
               DISPLAY 'EL NOMBRE ES: ' REG-NOMBRE
           END-IF.

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-SALIDA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS LEIDOS: " WSV-CONT-SALIDA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
