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
       01 FECHA.
           05 MES                  PIC 9(2).
           05 ANIO                 PIC 9(4).

       01 FECHA-TEXTUAL.
           05 MES-T                PIC 9(2).
           05 FILLER               PIC X(01) VALUE "-".
           05 ANIO-T               PIC 9(4).

       01 WSS-MES                  PIC X(2).
           88 MES-OK                       VALUE 'OK'.
           88 MES-INIC                     VALUE 'IN'.
       01 WSS-ANIO                 PIC X(4).
           88 ANIO-OK                      VALUE 'OK'.
           88 ANIO-INIC                    VALUE 'IN'.

       01 WSC-MES-MIN              PIC 9(02) VALUE 1.
       01 WSC-MES-MAX              PIC 9(02) VALUE 12.

       01 WSC-ANIO-MIN             PIC 9(04) VALUE 2000.
       01 WSC-ANIO-MAX             PIC 9(04) VALUE 2022.

       01 WSC-CANT-INTENTOS        PIC 9(01) VALUE 6.
       01 WSV-CONT-INTENTOS        PIC 9(1).

       LINKAGE SECTION.
       COPY REG-FECHA.
      *-----------------------
       PROCEDURE DIVISION USING REGISTRO-FECHA.
       00000-CONTROL.
           MOVE 0 TO WSV-CONT-INTENTOS
           SET WSS-RETORNO-ERROR TO TRUE
           PERFORM 10000-INGRESO-DE-FECHA
           PERFORM 20000-MUESTRA-FECHA
           PERFORM 30000-FIN-DE-RUTINA.
           
       10000-INGRESO-DE-FECHA.
           DISPLAY 'INGRESE LA FECHA (en formato mmaaaa):'
           ACCEPT FECHA
           PERFORM 11000-VALIDAR-FECHA.   

       20000-MUESTRA-FECHA.
           DISPLAY 'FECHA INGRESADA: ' FECHA
           MOVE FECHA TO WSV-FECHA-NUMERICA
           MOVE MES TO MES-T.
           MOVE ANIO TO ANIO-T
           MOVE FECHA-TEXTUAL TO WSV-FECHA-TEXTUAL
           SET WSS-RETORNO-OK TO TRUE.
       
       11000-VALIDAR-FECHA.
           SET MES-INIC TO TRUE
           SET ANIO-INIC TO TRUE    
           PERFORM UNTIL ((MES-OK AND ANIO-OK) 
                           OR (WSV-CONT-INTENTOS >= WSC-CANT-INTENTOS))
               PERFORM 11200-VALIDAR-MES
               PERFORM 11300-VALIDAR-ANIO  
               COMPUTE WSV-CONT-INTENTOS = WSV-CONT-INTENTOS + 1
           END-PERFORM
           PERFORM 11400-VALIDAR-CANTIDAD-INTENTOS.

       11200-VALIDAR-MES.
           IF MES >= WSC-MES-MIN AND MES <= WSC-MES-MAX
               SET MES-OK TO TRUE
           ELSE
               SET MES-INIC TO TRUE
               DISPLAY 'INGRESE EL MES NUEVAMENTE (en formato mm):'
               ACCEPT MES
           END-IF.
       
       11300-VALIDAR-ANIO.
           IF ANIO >= WSC-ANIO-MIN AND ANIO <= WSC-ANIO-MAX
               SET ANIO-OK TO TRUE
           ELSE
               SET ANIO-INIC TO TRUE
               DISPLAY 'INGRESE EL ANIO NUEVAMENTE (en formato aaaa):'
               ACCEPT ANIO
           END-IF.

       11400-VALIDAR-CANTIDAD-INTENTOS.
           IF WSV-CONT-INTENTOS >= WSC-CANT-INTENTOS 
           MOVE 'INTENTOS MAYOR A 6' TO WSV-TEXTO-STATUS
           SET WSS-RETORNO-ERROR TO TRUE
           PERFORM 30000-FIN-DE-RUTINA
           END-IF.

       30000-FIN-DE-RUTINA.
           GOBACK.
