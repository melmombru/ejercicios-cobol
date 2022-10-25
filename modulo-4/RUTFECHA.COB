      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Rutina fecha, valida la fecha ingresada
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. RUTFECHA.
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
           05 DIA          PIC 9(2).
           05 MES          PIC 9(2).
           05 ANIO         PIC 9(4).

       01  FECHA-TEXTUAL.
           05 DIA-T          PIC 9(2).
           05 FILLER       PIC X VALUE "/".
           05 MES-T          PIC 9(2).
           05 FILLER       PIC X VALUE "/".
           05 ANIO-T         PIC 9(4).

       01  WSS-DIA         PIC X(2).
               88 DIA-OK       VALUE 'OK'.
               88 DIA-INIC     VALUE 'IN'.
       01  WSS-MES         PIC X(2).
               88 MES-OK       VALUE 'OK'.
               88 MES-INIC     VALUE 'IN'.
       01  WSS-ANIO        PIC X(4).
               88 ANIO-OK      VALUE 'OK'.
               88 ANIO-INIC    VALUE 'IN'.

       77  INTENTOS        PIC 9(1) VALUE 0.

       LINKAGE SECTION.
       01  REGISTRO-LLAMADO.
           05 DATOS-RETORNO.
               10 WSV-FECHA-NUMERICA   PIC 9(08).
               10 WSV-FECHA-TEXTUAL    PIC X(20).
           05 STATUS-DATOS.
               10 WSV-STATUS-RETORNO   PIC 9(02) VALUE 00.
                   88 WSS-RETORNO-OK             VALUE 00.
                   88 WSS-RETORNO-ERROR          VALUE 11.
               10 WSV-TEXTO-STATUS     PIC X(20).
      *-----------------------
       PROCEDURE DIVISION USING REGISTRO-LLAMADO.
       00000-CONTROL.
           PERFORM INGRESO-DE-FECHA
           PERFORM MUESTRA-FECHA
           PERFORM FIN-DE-RUTINA.
           

       INGRESO-DE-FECHA.
           DISPLAY 'INGRESE LA FECHA (en formato ddmmaaaa):'
           ACCEPT FECHA
           PERFORM VALIDAR-FECHA.   

       MUESTRA-FECHA.
           DISPLAY 'FECHA INGRESADA: ' FECHA
           MOVE FECHA TO WSV-FECHA-NUMERICA
           MOVE DIA TO DIA-T.
           MOVE MES TO MES-T.
           MOVE ANIO TO ANIO-T
           MOVE FECHA-TEXTUAL TO WSV-FECHA-TEXTUAL
           SET WSS-RETORNO-OK TO TRUE.
       
       VALIDAR-DIA.
           IF DIA >= 1 AND DIA <= 31
               SET DIA-OK TO TRUE
           ELSE
               SET DIA-INIC TO TRUE
               DISPLAY 'INGRESE EL DIA NUEVAMENTE (en formato dd):'
               ACCEPT DIA 
           END-IF.

       VALIDAR-MES.
           IF MES >= 1 AND MES <= 12
               SET MES-OK TO TRUE
           ELSE
               SET MES-INIC TO TRUE
               DISPLAY 'INGRESE EL MES NUEVAMENTE (en formato mm):'
               ACCEPT MES
           END-IF.
       
       VALIDAR-ANIO.
           IF ANIO >= 1990 AND ANIO <= 2040
               SET ANIO-OK TO TRUE
           ELSE
               SET ANIO-INIC TO TRUE
               DISPLAY 'INGRESE EL ANIO NUEVAMENTE (en formato aaaa):'
               ACCEPT ANIO
           END-IF.
      
       VALIDAR-FECHA.
           SET DIA-INIC TO TRUE
           SET MES-INIC TO TRUE
           SET ANIO-INIC TO TRUE    
           PERFORM UNTIL ((DIA-OK AND MES-OK AND ANIO-OK) 
                           OR (INTENTOS > 6))
               PERFORM VALIDAR-DIA
               PERFORM VALIDAR-MES
               PERFORM VALIDAR-ANIO  
               COMPUTE INTENTOS = INTENTOS + 1
           END-PERFORM
           PERFORM VALIDAR-CANTIDAD-INTENTOS.

       VALIDAR-CANTIDAD-INTENTOS.
           IF INTENTOS > 6 
           DISPLAY 'INTENTOS MAYOR A 6'
           SET WSS-RETORNO-ERROR TO TRUE
           PERFORM FIN-DE-RUTINA
           END-IF.

       FIN-DE-RUTINA.
           GOBACK.
           

