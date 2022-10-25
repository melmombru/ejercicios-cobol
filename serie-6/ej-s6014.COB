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
       PROGRAM-ID. EJES6013.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA          ASSIGN TO DISK 'CUENTA.DAT'
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WSS-FS-ENTRADA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA.
       01 REG-ENTRADA.
           05 REG-NRO-CUENTA       PIC X(08).
           05 REG-COD-CLIENTE      PIC 9(08).
           05 REG-MONTO-CUENTA     PIC S9(15)V9(02).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ENTRADA           PIC X(02).
           88 WSS-FS-ENTRADA-OK    VALUE '00'.
           88 WSS-FS-ENTRADA-EOF   VALUE '10'.

       01 WSV-CONT-SALIDA         PIC S9(4) COMP.

       01 WSV-SEGUIR               PIC X(01).

       01 WSV-ENTRADA.
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-NRO-CUENTA.
               10 WSV-NRO-CUENTA-1 PIC X(02).
               10 FILLER           PIC X     VALUE ALL '-'.
               10 WSV-NRO-CUENTA-2 PIC X(05).
               10 FILLER           PIC X     VALUE ALL '-'.
               10 WSV-NRO-CUENTA-3 PIC X(01).
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-COD-CLIENTE     PIC 9(08).
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-MONTO-CUENTA.
               10 FILLER           PIC X     VALUE ALL '+'.
               10 WSV-ENTERO       PIC 9(15).
               10 FILLER           PIC X     VALUE ALL ','.
               10 WSV-DECIMAL      PIC 9(02).
           05 FILLER               PIC X     VALUE ALL '|'.

       01  TITULO0.
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(10) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(8) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(19) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
       
       01  TITULO1.
           05 FILLER           PIC X     VALUE ALL '|'.
           05 SERVICIO         PIC X(10) VALUE "NRO.CUENTA".
           05 FILLER           PIC X     VALUE ALL '|'.
           05 CUENTA           PIC X(8) VALUE "COD.CLI.".
           05 FILLER           PIC X     VALUE ALL '|'.
           05 DESCRIPCION      PIC X(19) VALUE " DISPONIBLE CUENTA".
           05 FILLER           PIC X     VALUE ALL '|'.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM 20000-LISTADO
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.

       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-SALIDA
           OPEN INPUT ENTRADA
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

       20000-LISTADO.
           DISPLAY TITULO0
           DISPLAY TITULO1
           DISPLAY TITULO0
           PERFORM 25000-LEER UNTIL WSS-FS-ENTRADA-EOF
           DISPLAY TITULO0.
           
       25000-LEER.
           IF NOT WSS-FS-ENTRADA-EOF
               READ ENTRADA
               IF WSS-FS-ENTRADA-OK
                   ADD 1 TO WSV-CONT-SALIDA
                   PERFORM 25500-ESCRIBIR-SERVICIO
      *        ELSE 
      *            DISPLAY 'ERROR EN EL READ DE ARCHIVO DE SALIDA'
      *            DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
               END-IF
           END-IF.
       
       25500-ESCRIBIR-SERVICIO.
           MOVE REG-NRO-CUENTA(1:2)   TO WSV-NRO-CUENTA-1
           MOVE REG-NRO-CUENTA(3:5)   TO WSV-NRO-CUENTA-2
           MOVE REG-NRO-CUENTA(8:1)   TO WSV-NRO-CUENTA-3
           MOVE REG-COD-CLIENTE    TO WSV-COD-CLIENTE
           MOVE REG-MONTO-CUENTA(1:15) TO WSV-ENTERO
           MOVE REG-MONTO-CUENTA(16:2) TO WSV-DECIMAL
           DISPLAY WSV-ENTRADA.
           
       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-SALIDA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-SALIDA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
