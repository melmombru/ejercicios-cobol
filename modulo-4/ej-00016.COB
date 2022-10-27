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
               10 MES-T            PIC 9(2).
               10 DIA-T            PIC 9(2).
               
               

       01  WSC-RUT-FECHA           PIC X(8) VALUE "RUTFECHA".

       01  WSC-RUT-ARCHIVO         PIC X(8) VALUE "RUTARCH".
       
       COPY REG-LLAMADO.

       COPY REG-ARCHIVO.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.

       10000-ABRIR-ARCHIVO.
           MOVE "OOU" TO WSS-OPCION
           CALL WSC-RUT-ARCHIVO USING REGISTRO-ARCHIVO
           IF WSS-ST-OK
               PERFORM INGRESO-DE-DATOS
           ELSE
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.
           
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
           CALL WSC-RUT-FECHA USING REGISTRO-LLAMADO
           PERFORM UNTIL WSS-RETORNO-OK
               DISPLAY "error"
               CALL WSC-RUT-FECHA USING REGISTRO-LLAMADO
           END-PERFORM
           MOVE WSV-FECHA-NUMERICA(1:2) TO DIA-T
           MOVE WSV-FECHA-NUMERICA(3:2) TO MES-T
           MOVE WSV-FECHA-NUMERICA(5:4) TO ANIO-T.
       
       30000-CERRAR-ARCHIVO.
           MOVE "CLO" TO WSS-OPCION
           CALL WSC-RUT-ARCHIVO USING REGISTRO-ARCHIVO
           IF NOT WSS-ST-OK
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.

       20000-ESCRITURA.
           MOVE "WRI" TO WSS-OPCION
           MOVE WSV-ENTRADA TO REG-ARCH-E
           CALL WSC-RUT-ARCHIVO USING REGISTRO-ARCHIVO
           IF WSS-ST-OK
               ADD 1 TO WSV-CONT-ENTRADA
           ELSE
               PERFORM 0002-MOSTRAR-RETORNO
           END-IF.

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-ENTRADA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-ENTRADA
           END-IF.

       0002-MOSTRAR-RETORNO.
           DISPLAY WSV-ST-TEXTO
           DISPLAY WSV-ST-RETORNO.

       FIN-DEL-PROGRAMA.
           STOP RUN.
