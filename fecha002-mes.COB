      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. WSV-FECHA002.
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
       01  WSV-FECHA.
           05 WSV-DIA          PIC 9(2).
           05 WSV-MES          PIC 9(2).
           05 WSV-ANIO         PIC 9(4).
       
       01  WSV-MESES.
           05 FILLER       PIC X(10) VALUE 'ENERO     '.
           05 FILLER       PIC X(10) VALUE 'FEBRERO   '.
           05 FILLER       PIC X(10) VALUE 'MARZO     '.
           05 FILLER       PIC X(10) VALUE 'ABRIL     '.
           05 FILLER       PIC X(10) VALUE 'MAYO      '.
           05 FILLER       PIC X(10) VALUE 'JUNIO     '.
           05 FILLER       PIC X(10) VALUE 'JULIO     '.
           05 FILLER       PIC X(10) VALUE 'AGOSTO    '.
           05 FILLER       PIC X(10) VALUE 'SEPTIEMBRE'.
           05 FILLER       PIC X(10) VALUE 'OCTUBRE   '.
           05 FILLER       PIC X(10) VALUE 'NOVIEMBRE '.
           05 FILLER       PIC X(10) VALUE 'DICIEMBRE '.

       01  WST-MESES REDEFINES WSV-MESES.
           05 WST-MES      PIX X(10) OCCURS 12.

       01  WSS-WSV-DIA         PIC X(2).
               88 WSS-DIA-OK       VALUE 'OK'.
               88 WSS-DIA-INIC     VALUE 'IN'.
       01  WSS-WSV-MES         PIC X(2).
               88 WSS-MES-OK       VALUE 'OK'.
               88 WSS-MES-INIC     VALUE 'IN'.
       01  WSS-WSV-ANIO        PIC X(4).
               88 WSS-ANIO-OK      VALUE 'OK'.
               88 WSS-ANIO-INIC    VALUE 'IN'.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-FECHA
           PERFORM MUESTRA-FECHA
           PERFORM FIN-DEL-PROGRAMA.
           

       INGRESO-DE-WSV-FECHA.
           DISPLAY 'INGRESE LA FECHA (en formato ddmmaaaa):'
           ACCEPT WSV-FECHA
           PERFORM VALIDAR-FECHA.   

       MUESTRA-FECHA.
           DISPLAY 'FECHA INGRESADA: ' WSV-FECHA.
       
       VALIDAR-FECHA-DIA.
           IF WSV-DIA >= 1 AND WSV-DIA <= 31
               SET WSS-DIA-OK TO TRUE
           ELSE
               SET WSS-DIA-INIC TO TRUE
               DISPLAY 'INGRESE EL DIA NUEVAMENTE (en formato dd):'
               ACCEPT WSV-DIA 
           END-IF.

       VALIDAR-FECHA-MES.
           IF WSV-MES >= 1 AND WSV-MES <= 12
               SET WSS-MES-OK TO TRUE
           ELSE
               SET WSS-MES-INIC TO TRUE
               DISPLAY 'INGRESE EL MES NUEVAMENTE (en formato mm):'
               ACCEPT WSV-MES
           END-IF.
       
       VALIDAR-FECHA-ANIO.
           IF WSV-ANIO >= 1990 AND WSV-ANIO <= 2040
               SET WSS-ANIO-OK TO TRUE
           ELSE
               SET WSS-ANIO-INIC TO TRUE
               DISPLAY 'INGRESE EL WSV-ANIO NUEVAMENTE (en formato aaaa):'
               ACCEPT WSV-ANIO
           END-IF.
      
       VALIDAR-FECHA.
           SET DIA-INIC TO TRUE
           SET WSS-MES-INIC TO TRUE
           SET WSS-ANIO-INIC TO TRUE    
           PERFORM UNTIL (WSS-DIA-OK AND WSS- AND ANIO-OK)
               PERFORM VALIDAR-FECHA-DIA
               PERFORM VALIDAR-FECHA-MES
               PERFORM VALIDAR-FECHA-ANIO  
           END-PERFORM.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           

