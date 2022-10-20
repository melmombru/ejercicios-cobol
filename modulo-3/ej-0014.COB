      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: 

      *Ejercicio 0014 escritura indexado

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE014.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT SALIDA1          ASSIGN TO DISK './DatosDePersonas.dat'
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS RANDOM
                               RECORD KEY IS REG-CLAVE
                               FILE STATUS IS WSS-FS-SALIDA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD SALIDA1.
       01 REG-SALIDA1.
           05 REG-CLAVE            PIC X(2).
           05 REG-NOMBRE           PIC X(30).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-SALIDA           PIC X(02).
           88 WSS-FS-SALIDA-OK    VALUE '00'.
           88 WSS-FS-SALIDA-EOF   VALUE '10'.

       01 WSV-CONT-SALIDA         PIC S9(4) COMP.

       01 WSV-SEGUIR               PIC X(02).

       01 WSV-ENTRADA.
           05 WSV-CLAVE            PIC X(02).
           05 WSV-NOMBRE           PIC X(30).

       77 I                        PIC 9(02).
      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM INGRESO-DE-DATOS
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.
           
       INGRESO-DE-DATOS.
           PERFORM UNTIL WSV-SEGUIR = '99'
               DISPLAY "INGRESAR CLAVE: "
               ACCEPT WSV-CLAVE
               DISPLAY "INGRESAR NOMBRE: "
               ACCEPT WSV-NOMBRE
               PERFORM 20000-ESCRITURA
               DISPLAY "DESEA SEGUIR? "
               ACCEPT WSV-SEGUIR
           END-PERFORM.

       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-SALIDA
           OPEN I-O SALIDA1
           IF NOT WSS-FS-SALIDA-OK
               DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
               PERFORM FIN-DEL-PROGRAMA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE SALIDA1
           IF NOT WSS-FS-SALIDA-OK
               DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.

       20000-ESCRITURA.
           PERFORM 15000-LEER-Y-ESCRIBIR.

       15000-LEER-Y-ESCRIBIR.
           REWRITE REG-SALIDA1 FROM WSV-ENTRADA.
           IF WSS-FS-SALIDA-OK
               ADD 1 TO WSV-CONT-SALIDA
           END-IF.

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-SALIDA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-SALIDA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
