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

       SELECT ENTRADA          ASSIGN TO DISK 'SERVICIO.DAT'
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WSS-FS-ENTRADA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA.
       01 REG-ENTRADA.
           05 REG-COD-SERVICIO     PIC X(03).
           05 REG-NRO-CUENTA       PIC 9(08).
           05 REG-DESCRIP-SERV     PIC X(30).
           05 REG-PERIODO-FACT     PIC X(06).
           05 REG-MONTO-FACTURA    PIC S9(05)V9(02).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ENTRADA           PIC X(02).
           88 WSS-FS-ENTRADA-OK    VALUE '00'.
           88 WSS-FS-ENTRADA-EOF   VALUE '10'.

       01 WSV-CONT-SALIDA         PIC S9(4) COMP.

       01 WSV-SEGUIR               PIC X(01).

       01 WSV-ENTRADA.
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-COD-SERVICIO     PIC X(05).
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-NRO-CUENTA.
               10 WSV-NRO-CUENTA-1 PIC 9(02).
               10 FILLER           PIC X     VALUE ALL '-'.
               10 WSV-NRO-CUENTA-2 PIC 9(05).
               10 FILLER           PIC X     VALUE ALL '-'.
               10 WSV-NRO-CUENTA-3 PIC 9(01).
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-DESCRIP-SERV     PIC X(40).
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-PERIODO-FACT.
               10 WSV-MES          PIC 9(02).
               10 FILLER           PIC X     VALUE ALL '/'.
               10 WSV-ANIO         PIC 9(04).
           05 FILLER               PIC X     VALUE ALL '|'.
           05 WSV-MONTO-FACTURA.
               10 FILLER           PIC X     VALUE ALL '+'.
               10 WSV-ENTERO       PIC 9(05).
               10 FILLER           PIC X     VALUE ALL ','.
               10 WSV-DECIMAL      PIC 9(02).
           05 FILLER               PIC X     VALUE ALL '|'.

       01  TITULO0.
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(5) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(10) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(40) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(7) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
           05 FILLER           PIC X(9) VALUE ALL "-".
           05 FILLER           PIC X VALUE "*".
       
       01  TITULO1.
           05 FILLER           PIC X     VALUE ALL '|'.
           05 SERVICIO         PIC X(05) VALUE "SERV.".
           05 FILLER           PIC X     VALUE ALL '|'.
           05 CUENTA           PIC X(10) VALUE " CUENTA".
           05 FILLER           PIC X     VALUE ALL '|'.
           05 DESCRIPCION      PIC X(40) VALUE "  DESCRIPCION      ".
           05 FILLER           PIC X     VALUE ALL '|'.
           05 PERIODO          PIC X(7) VALUE "PERIODO".
           05 FILLER           PIC X     VALUE ALL '|'.
           05 MONTO            PIC X(9) VALUE " MONTO ".
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
           MOVE REG-COD-SERVICIO  TO WSV-COD-SERVICIO
           MOVE REG-NRO-CUENTA(1:2)   TO WSV-NRO-CUENTA-1
           MOVE REG-NRO-CUENTA(3:5)   TO WSV-NRO-CUENTA-2
           MOVE REG-NRO-CUENTA(8:1)   TO WSV-NRO-CUENTA-3
           MOVE REG-DESCRIP-SERV  TO WSV-DESCRIP-SERV
           MOVE REG-PERIODO-FACT(5:2)  TO WSV-MES
           MOVE REG-PERIODO-FACT(1:4)  TO WSV-ANIO
           MOVE REG-MONTO-FACTURA(1:5) TO WSV-ENTERO
           MOVE REG-MONTO-FACTURA(6:2) TO WSV-DECIMAL
           DISPLAY WSV-ENTRADA.
           
       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-SALIDA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-SALIDA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
