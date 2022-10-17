      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES2104.
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
       01  WSV-FUNCION-MATEMATICA.
           05 WSV-NUMERO-1     PIC 9(04).
           05 FILLER           PIC X(01).
           05 WSV-OPERADOR     PIC X(01).
           05 FILLER           PIC X(01).
           05 WSV-NUMERO-2     PIC 9(04).

       77  WSV-RESULTADO       PIC 9(8)V9(4).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM REALIZAR-OPERACION
           PERFORM MOSTRAR-RESULTADO.
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           DISPLAY "INGRESE FUNCION MATEMATICA:"
           ACCEPT WSV-FUNCION-MATEMATICA.
       
       REALIZAR-OPERACION.
           EVALUATE WSV-OPERADOR
           WHEN "+"
               COMPUTE WSV-RESULTADO = WSV-NUMERO-1 + WSV-NUMERO-2   
           WHEN "-"
               COMPUTE WSV-RESULTADO = WSV-NUMERO-1 - WSV-NUMERO-2   
           WHEN "*"
               COMPUTE WSV-RESULTADO = WSV-NUMERO-1 * WSV-NUMERO-2   
           WHEN "/"
               COMPUTE WSV-RESULTADO = WSV-NUMERO-1 / WSV-NUMERO-2
           WHEN OTHER
               DISPLAY "OPERADOR INVALIDO"
           END-EVALUATE.

       MOSTRAR-RESULTADO.
           DISPLAY "RESULTADO: " WSV-RESULTADO.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           