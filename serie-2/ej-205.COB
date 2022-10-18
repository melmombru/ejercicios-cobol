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
       01  WSV-FUNCION-MATEMATICA PIC X(35).
       
       01 WS-CNT PIC 99 VALUE 0.
       01 WSV-FUNCION.
           05 WSV-OPERANDO 
                   OCCURS 1 TO 20 TIMES DEPENDING ON WS-CNT PIC X.

       01  WSV-FUNCION.
           05 WSV-NUMERO-1     PIC 9(10).
           05 FILLER           PIC X(01).
           05 WSV-OPERADOR     PIC X(01).
           05 FILLER           PIC X(01).
           05 WSV-NUMERO-2     PIC 9(04).

       77  WSV-I               PIC 9(02).
       77  WSV-J               PIC 9(02) VALUE 1.
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
           ACCEPT WSV-FUNCION-MATEMATICA
           PERFORM ELIMINAR-ESPACIOS.

       ELIMINAR-ESPACIOS.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > 35
               IF WSV-FUNCION-MATEMATICA(WSV-I:1) = " " THEN
                   CONTINUE
               ELSE
                   MOVE WSV-FUNCION-MATEMATICA(WSV-I:1) TO 
                   WSV-OPERANDO(WSV-J)
                   ADD 1 TO WSV-J
                   ADD 1 TO WS-CNT
               END-IF
           END-PERFORM
           DISPLAY WSV-FUNCION.
       
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
           