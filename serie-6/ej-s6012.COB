      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: 

      *escritura indexado

      *Código de servicio (Alfanumérico de 3 caracteres).
      *Número de cuenta (8 enteros).
      *Descripción del servicio (Alfanumérico de 30 caracteres).
      *Periodo facturado (Alfanumérico de 6 caracteres – Formato AAAAMM)
      *Monto facturado (5 enteros y 2 decimales con signo).

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES6012.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT SALIDA1          ASSIGN TO DISK 'CUENTA.DAT'
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WSS-FS-SALIDA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD SALIDA1.
       01 REG-SALIDA1.
           05 REG-NRO-CUENTA       PIC X(08).
           05 REG-COD-CLIENTE      PIC 9(08).
           05 REG-MONTO-CUENTA     PIC S9(15)V9(02).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-SALIDA            PIC X(02).
           88 WSS-FS-SALIDA-OK     VALUE '00'.
           88 WSS-FS-SALIDA-EOF    VALUE '10'.

       01 WSV-CONT-SALIDA          PIC S9(4) COMP.

       01 WSV-SEGUIR               PIC X(01).

       01 WSV-ENTRADA.
           05 WSV-NRO-CUENTA       PIC X(08).
           05 WSV-COD-CLIENTE      PIC 9(08).
           05 WSV-MONTO-CUENTA     PIC S9(15)V9(02).

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM INGRESO-DE-DATOS
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.
           
       INGRESO-DE-DATOS.
           PERFORM UNTIL WSV-SEGUIR = 'N' OR WSV-SEGUIR = 'n'
               DISPLAY "INGRESAR NRO DE CUENTA: " 
               ACCEPT WSV-NRO-CUENTA
               DISPLAY "INGRESAR CODIGO DE CLIENTE: "
               ACCEPT WSV-COD-CLIENTE
               DISPLAY "INGRESAR MONTO DE LA CUENTA: "
               ACCEPT WSV-MONTO-CUENTA

               PERFORM 20000-ESCRITURA
               DISPLAY "DESEA SEGUIR? "
               ACCEPT WSV-SEGUIR
           END-PERFORM.

       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-SALIDA
           OPEN OUTPUT SALIDA1
           IF NOT WSS-FS-SALIDA-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
               PERFORM FIN-DEL-PROGRAMA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE SALIDA1
           IF NOT WSS-FS-SALIDA-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.

       20000-ESCRITURA.
           WRITE REG-SALIDA1 FROM WSV-ENTRADA.
           IF WSS-FS-SALIDA-OK
               ADD 1 TO WSV-CONT-SALIDA
           ELSE 
               DISPLAY 'ERROR EN EL WRITE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-SALIDA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-SALIDA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
