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
       01  WSV-PERSONA.
           05 WSV-NOMBRE       PIC X(8).
           05 WSV-APELLIDO     PIC X(8).
           05 WSV-DNI          PIC X(10).
           05 WSV-TELEFONO     PIC X(11).
           05 WSV-DIRECCION    PIC X(20).
           05 WSV-ESTADO-CIVIL PIC X(01).
               88 WSS-CASADO       VALUE "C".
               88 WSS-SOLTERO      VALUE "S".
               88 WSS-VIUDO        VALUE "V".
               88 WSS-DIVORCIADO   VALUE "D".
           05 WSV-HIJOS        PIC X(01).
               88 WSS-SI-HIJOS     VALUE "S".
               88 WSS-NO-HIJOS     VALUE "N".
           05 WSV-SANGUINEO.  
               10 WSV-GRUPO    PIC X(01).
                   88 WSS-GRUPO-A  VALUE "A".
                   88 WSS-GRUPO-B  VALUE "B".
                   88 WSS-GRUPO-0  VALUE "0".
               10 WSV-FACTOR   PIC X(01).
                   88 WSS-FACTOR-POSITIVO  VALUE "+".
                   88 WSS-FACTOR-NEGATIVO  VALUE "-".
       
       77  WSV-SEGUIR          PIC X(01).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-PERSONAS
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-PERSONA.
           DISPLAY "INGRESE NOMBRE: "
           ACCEPT WSV-NOMBRE
           DISPLAY "INGRESE APELLIDO: "
           ACCEPT WSV-APELLIDO
           DISPLAY "INGRESE DNI: "
           ACCEPT WSV-DNI
           DISPLAY "INGRESE TELEFONO: "
           ACCEPT WSV-TELEFONO
           DISPLAY "INGRESE DIRECCION: "
           ACCEPT WSV-DIRECCION
           DISPLAY "INGRESE ESTADO CIVIL: "
           ACCEPT WSV-ESTADO-CIVIL
           DISPLAY "TIENE HJOS: (S/N)"
           ACCEPT WSV-HIJOS
           DISPLAY "INGRESE GRUPO Y FACTOR SANGUINEO: "
           ACCEPT WSV-SANGUINEO.
           
       MOSTRAR-PERSONA.
           DISPLAY WSV-APELLIDO ", " WSV-NOMBRE ":".
           DISPLAY "DNI: " WSV-DNI
           DISPLAY "TELEFONO: " WSV-TELEFONO
           DISPLAY "DIRECCION: " WSV-DIRECCION
           DISPLAY "ESTADO CIVIL: " WSV-ESTADO-CIVIL
           DISPLAY "TIENE HIJOS: " WSV-HIJOS
           DISPLAY "GRUPO SANGUINEO: " WSV-SANGUINEO.

       INGRESO-DE-PERSONAS.
           MOVE "Y" TO WSV-SEGUIR
           PERFORM UNTIL WSV-SEGUIR = "N" OR WSV-SEGUIR = "n"
               PERFORM INGRESO-DE-PERSONA
               PERFORM MOSTRAR-PERSONA
               DISPLAY "SEGUIR INGRESANDO? " 
               "(INGRESE N/n SI NO DESEA CONTINUAR)"
               ACCEPT WSV-SEGUIR
           END-PERFORM. 
      
       FIN-DEL-PROGRAMA.
           STOP RUN.
           