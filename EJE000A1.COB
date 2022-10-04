      ******************************************************************
      * Author: Guillermo
      * Date: 2021 08
      * DESCRIPCION: Programa base, con entrada de un dato
      *              y  de salida el saludo
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE000A1.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------
       DATA DIVISION.
       FILE SECTION.

      *-----------------------
       WORKING-STORAGE SECTION.

       01  WSV-ALUMNO                  PIC X(20).


      *-----------------------
       PROCEDURE DIVISION.

       0000-CUERPO-DEL-PROG.

           PERFORM 1000-ENTRAR-ALUMNO

           PERFORM 2000-MOSTRAR

           STOP RUN.

       1000-ENTRAR-ALUMNO.
           DISPLAY 'QUIEN VA A ESTUDIAR ? '
           ACCEPT  WSV-ALUMNO .
       2000-MOSTRAR.
           DISPLAY  '                  Bienvenido al curso ' WSV-ALUMNO.
