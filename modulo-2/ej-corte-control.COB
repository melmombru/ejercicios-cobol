      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE002M2.
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
       01  WSV-CLIENTE.
           05 WSV-NRO-CLIENTE      PIC 9(02).
           05 WSV-MONTO-CLIENTE    PIC S9(03).


       01  WSV-INDICES.         
           05 I                    PIC 9(1).
           05 J                    PIC 9(1).

       77  WSV-CLIENTE-ANTERIOR    PIC 9(02).

       77  WSV-MONTO-TOTAL         PIC S9(05). 

      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM CORTE-DE-CONTROL
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           DISPLAY "INGRESE NRO DE CLIENTE"
           ACCEPT WSV-NRO-CLIENTE
           DISPLAY "INGRESE MONTO DEL CLIENTE"
           ACCEPT WSV-MONTO-CLIENTE.

      *MUESTRA POR PANTALLA EL MONTO ACUMULADO DEL CLIENTE      
       MOSTRAR-MONTO.
           DISPLAY "MONTO TOTAL " WSV-MONTO-TOTAL
           DISPLAY " ".

      *CORTE DE CONTROL POR NRO CLIENTE, FINALIZA INGRESO CUANDO EL 
      *NRO DE CLIENTE ES 0
       CORTE-DE-CONTROL.
           PERFORM INGRESO-DE-DATOS
           MOVE WSV-NRO-CLIENTE TO WSV-CLIENTE-ANTERIOR
           PERFORM UNTIL WSV-NRO-CLIENTE = 0
               MOVE ZEROES TO WSV-MONTO-TOTAL
               PERFORM UNTIL ((WSV-NRO-CLIENTE = 0) OR 
               (WSV-NRO-CLIENTE NOT = WSV-CLIENTE-ANTERIOR))
                   COMPUTE WSV-MONTO-TOTAL = WSV-MONTO-CLIENTE + 
                   WSV-MONTO-TOTAL 
                   PERFORM INGRESO-DE-DATOS
               END-PERFORM
               DISPLAY "CLIENTE " WSV-CLIENTE-ANTERIOR
               PERFORM MOSTRAR-MONTO
               MOVE WSV-NRO-CLIENTE TO WSV-CLIENTE-ANTERIOR
           END-PERFORM.
           
       FIN-DEL-PROGRAMA.
           STOP RUN.
         