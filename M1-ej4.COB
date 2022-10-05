      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio M1-3
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE40M1.
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
       01  FECHA.
           05 DIA         PIC 9(2).
           05 MES         PIC 9(2).
           05 ANIO        PIC 9(4).

       01  WSS-FECHA.
           05 WSS-DIA         PIC X(2).
               88 DIA-OK       VALUE 'OK'.
               88 DIA-INIC    VALUE 'IN'.
           05 WSS-MES         PIC X(2).
               88 MES-OK       VALUE 'OK'.
               88 MES-INIC    VALUE 'IN'.
           05 WSS-ANIO        PIC X(4).
               88 ANIO-OK      VALUE 'OK'.
               88 ANIO-INIC   VALUE 'IN'.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-FECHA
           PERFORM MUESTRA-FECHA
           PERFORM FIN-DEL-PROGRAMA.
           

       INGRESO-DE-FECHA.
           DISPLAY 'INGRESE LA FECHA (en formato ddmmaaaa):'
           ACCEPT FECHA
           DISPLAY FECHA
           PERFORM VALIDAR-FECHA.   

       MUESTRA-FECHA.
           DISPLAY 'FECHA INGRESADA: ' FECHA.
       
       VALIDAR-FECHA-DIA.
           IF DIA >= 1 AND DIA <= 31
               SET DIA-OK TO TRUE
           ELSE
               SET DIA-INIC TO TRUE
           END-IF
           PERFORM UNTIL DIA-OK
               DISPLAY 'INGRESE EL DIA NUEVAMENTE (en formato dd):'
               ACCEPT DIA 
               IF DIA >= 1 AND DIA <= 31
                   SET DIA-OK TO TRUE
               ELSE
                   SET DIA-INIC TO TRUE
               END-IF  
           END-PERFORM.

       VALIDAR-FECHA-MES.
           IF MES >= 1 AND MES <= 12
               SET MES-OK TO TRUE
           ELSE
               SET MES-INIC TO TRUE
           END-IF
           PERFORM UNTIL MES-OK
               DISPLAY 'INGRESE EL MES NUEVAMENTE (en formato mm):'
               ACCEPT MES 
               IF MES >= 1 AND MES <= 12
                   SET MES-OK TO TRUE
               ELSE
                   SET MES-INIC TO TRUE
               END-IF   
           END-PERFORM.
       
       VALIDAR-FECHA-ANIO.
           IF ANIO >= 1990 AND ANIO <= 2040
               SET ANIO-OK TO TRUE
           ELSE
               SET ANIO-INIC TO TRUE
           END-IF
           PERFORM UNTIL ANIO-OK
               DISPLAY 'INGRESE EL ANIO NUEVAMENTE (en formato aaaa):'
               ACCEPT ANIO 
               IF ANIO >= 1990 AND ANIO <= 2040
                   SET ANIO-OK TO TRUE
               ELSE
                   SET ANIO-INIC TO TRUE
               END-IF    
           END-PERFORM.
      
       VALIDAR-FECHA.
           SET DIA-INIC TO TRUE
           SET MES-INIC TO TRUE
           SET ANIO-INIC TO TRUE
           PERFORM VALIDAR-FECHA-DIA
           PERFORM VALIDAR-FECHA-MES
           PERFORM VALIDAR-FECHA-ANIO.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           

