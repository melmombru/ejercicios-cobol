      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: PAISs

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PAISS.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT PAIS                     ASSIGN TO DISK 
                                       'nacionalidades.dat'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-PAIS.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD PAIS.
       01 REG-PAIS.
           05 REG-NRO-PAIS             PIC 9(03).
           05 REG-DESCRIP-PAIS         PIC X(20).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-PAIS                  PIC X(02).
           88 WSS-FS-PAIS-OK                   VALUE '00'.
           88 WSS-FS-PAIS-EOF                  VALUE '10'.

       01 WSV-CONT-PAIS                PIC S9(4) COMP.

       01 WSV-SEGUIR                   PIC X(01).

       01 WSV-PAIS.
           05 WSV-NRO-PAIS             PIC 9(03).
           05 WSV-DESCRIP-PAIS         PIC X(20).

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM INGRESO-DE-DATOS
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.
           
       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-PAIS
           OPEN OUTPUT PAIS
           IF NOT WSS-FS-PAIS-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-PAIS
               PERFORM FIN-DEL-PROGRAMA
           END-IF.

       INGRESO-DE-DATOS.
           PERFORM UNTIL WSV-SEGUIR = 'N' OR WSV-SEGUIR = 'n'
               DISPLAY "INGRESAR NRO DE PAIS: "
               ACCEPT WSV-NRO-PAIS
               DISPLAY "INGRESAR DESCRIPCION DE PAIS: "
               ACCEPT WSV-DESCRIP-PAIS

               PERFORM 20000-ESCRITURA
               DISPLAY "DESEA SEGUIR? "
               ACCEPT WSV-SEGUIR
           END-PERFORM.

       20000-ESCRITURA.
           WRITE REG-PAIS FROM WSV-PAIS.
           IF WSS-FS-PAIS-OK
               ADD 1 TO WSV-CONT-PAIS
           ELSE 
               DISPLAY 'ERROR EN EL WRITE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-PAIS
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE PAIS
           IF NOT WSS-FS-PAIS-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-PAIS
           END-IF.

       

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-PAIS = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-PAIS
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
