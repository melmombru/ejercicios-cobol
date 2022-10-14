      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2101.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------
       DATA DIVISION.

      *-----------------------
       WORKING-STORAGE SECTION.
       01  WSV-ALUMNO.
           05 WSV-NOMBRE       PIC X(8).
           05 WSV-APELLIDO     PIC X(8).
           05 WST-NOTAS.
               10 WST-NOTA     PIC 9(02) OCCURS 3 TIMES.

       01  WSV-CONTADORES.
           05 I                PIC 9(02).
       
       77  WSV-PROMEDIO        PIC 9(2)v9(2).
       
       77  WSV-ACUMULADOR      PIC 9(02).

       77  WSV-SEGUIR          PIC X(01).

       01  WSC-DIM    PIC 9(02) VALUE 3.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM PROMEDIO-ALUMNOS
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-ALUMNOS.
           DISPLAY "INGRESE NOMBRE: "
           ACCEPT WSV-NOMBRE
           DISPLAY "INGRESE APELLIDO: "
           ACCEPT WSV-APELLIDO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM
               DISPLAY "INGRESE NOTA " I ": "
               ACCEPT WST-NOTA(I) 
           END-PERFORM.
           
       CALCULAR-PROMEDIO.
           MOVE ZEROES TO WSV-PROMEDIO
           MOVE ZEROES TO WSV-ACUMULADOR
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-DIM
               ADD WST-NOTA(I) TO WSV-ACUMULADOR
           END-PERFORM
           COMPUTE WSV-PROMEDIO = WSV-ACUMULADOR / WSC-DIM.

       MOSTRAR-ALUMNO.
           DISPLAY WSV-APELLIDO ", " WSV-NOMBRE ": PROMEDIO " 
           WSV-PROMEDIO(1:2) "," WSV-PROMEDIO(3:2).

       PROMEDIO-ALUMNOS.
           MOVE "Y" TO WSV-SEGUIR
           PERFORM UNTIL WSV-SEGUIR = "N" OR WSV-SEGUIR = "n"
               PERFORM INGRESO-DE-ALUMNOS
               PERFORM CALCULAR-PROMEDIO
               PERFORM MOSTRAR-ALUMNO
               DISPLAY "SEGUIR INGRESANDO? " 
               "(INGRESE N/n SI NO DESEA CONTINUAR)"
               ACCEPT WSV-SEGUIR
           END-PERFORM. 
      
       FIN-DEL-PROGRAMA.
           STOP RUN.
           