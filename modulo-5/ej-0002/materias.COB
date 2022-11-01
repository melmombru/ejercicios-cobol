      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: materias

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MATERIAS.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT MATERIA                  ASSIGN TO DISK 'materias.dat'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-MATERIA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD MATERIA.
       01 REG-MATERIA.
           05 REG-NRO-MATERIA          PIC 9(02).
           05 REG-DESCRIP-MATERIA      PIC X(25).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-MATERIA               PIC X(02).
           88 WSS-FS-MATERIA-OK                VALUE '00'.
           88 WSS-FS-MATERIA-EOF               VALUE '10'.

       01 WSV-CONT-MATERIA             PIC S9(4) COMP.

       01 WSV-SEGUIR                   PIC X(01).

       01 WSV-MATERIA.
           05 WSV-NRO-MATERIA          PIC 9(02).
           05 WSV-DESCRIP-MATERIA      PIC X(25).

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM INGRESO-DE-DATOS
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.
           
       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-MATERIA
           OPEN OUTPUT MATERIA
           IF NOT WSS-FS-MATERIA-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-MATERIA
               PERFORM FIN-DEL-PROGRAMA
           END-IF.

       INGRESO-DE-DATOS.
           PERFORM UNTIL WSV-SEGUIR = 'N' OR WSV-SEGUIR = 'n'
               DISPLAY "INGRESAR NRO DE MATERIA: "
               ACCEPT WSV-NRO-MATERIA
               DISPLAY "INGRESAR DESCRIPCION DE MATERIA: "
               ACCEPT WSV-DESCRIP-MATERIA

               PERFORM 20000-ESCRITURA
               DISPLAY "DESEA SEGUIR? "
               ACCEPT WSV-SEGUIR
           END-PERFORM.

       20000-ESCRITURA.
           WRITE REG-MATERIA FROM WSV-MATERIA.
           IF WSS-FS-MATERIA-OK
               ADD 1 TO WSV-CONT-MATERIA
           ELSE 
               DISPLAY 'ERROR EN EL WRITE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-MATERIA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE MATERIA
           IF NOT WSS-FS-MATERIA-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE SALIDA'
               DISPLAY 'FILE STATUS' WSS-FS-MATERIA
           END-IF.

       

       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-MATERIA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS ESCRITOS: "WSV-CONT-MATERIA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
