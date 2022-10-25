      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: 

      *Ejercicio 0013 escritura indexado

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE013.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA          ASSIGN TO DISK './DatosDePersonas.dat'
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS RANDOM
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

       01 WSV-CONT-ENTRADA         PIC S9(4) COMP.

       01 WSV-SEGUIR               PIC X(02).

       01 WSV-ENTRADA.
           05 WSV-CLAVE            PIC X(02).
           05 WSV-NOMBRE           PIC X(30).
           05 WSV-FECHA.
               10 ANIO-T           PIC 9(4).
               10 FILLER           PIC X VALUE "-".
               10 MES-T            PIC 9(2).
               10 FILLER           PIC X VALUE "-".
               10 DIA-T            PIC 9(2).
               
               

       01  WSC-RUTINA              PIC X(8) VALUE "RUTFECHA".
       
       COPY REG-LLAMADO.

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
               PERFORM INGRESO-DE-FECHA
               PERFORM 20000-ESCRITURA

               DISPLAY "DESEA SEGUIR? "
               ACCEPT WSV-SEGUIR
           END-PERFORM.

       INGRESO-DE-FECHA.
           CALL WSC-RUTINA USING REGISTRO-LLAMADO
           PERFORM UNTIL WSS-RETORNO-OK
               DISPLAY "error"
               CALL WSC-RUTINA USING REGISTRO-LLAMADO
           END-PERFORM
           MOVE WSV-FECHA-NUMERICA(1:2) TO DIA-T
           MOVE WSV-FECHA-NUMERICA(3:2) TO MES-T
           MOVE WSV-FECHA-NUMERICA(5:4) TO ANIO-T.
           

       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN OUTPUT ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
               PERFORM FIN-DEL-PROGRAMA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.

       20000-ESCRITURA.
           PERFORM 15000-LEER-Y-ESCRIBIR.

       15000-LEER-Y-ESCRIBIR.
           WRITE REG-ENTRADA FROM WSV-ENTRADA.
           IF WSS-FS-ENTRADA-OK
               ADD 1 TO WSV-CONT-ENTRADA
           ELSE 
               DISPLAY 'ERROR EN EL WRITE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-ENTRADA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-ENTRADA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
