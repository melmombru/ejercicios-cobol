      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio 19
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE190S1.
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
       01  CARGA.     
           02 IMPORTES-1      PIC 9(6).
           02 IMPORTES-2      PIC 9(6).
           02 IMPORTES-3      PIC 9(6).
           02 IMPORTES-4      PIC 9(6).
           02 IMPORTES-5      PIC 9(6).
       
       01  RESULTADO.
           02 TOTAL-DISPLAY   PIC X(7) VALUE ' TOTAL '.
           02 TOTAL           PIC 9(10).
           02 IVA-DISPLAY     PIC X(5) VALUE ' IVA '.
           02 IVA             PIC 9(7).
       
       77  WSC-PORCENTAJE-IVA PIC 9V9(2) VALUE 0.21.
      *-----------------------
       PROCEDURE DIVISION.
       00000-PROCESO.
           PERFORM 10000-CARGA
           PERFORM 20000-CALCULO
           PERFORM 30000-RESULTADO
           
           STOP RUN.

       10000-CARGA.
           DISPLAY 'INGRESE VALOR DEL PRIMER IMPORTE:'
           ACCEPT IMPORTES-1
           DISPLAY 'INGRESE VALOR DEL SEGUNDO IMPORTE:'
           ACCEPT IMPORTES-2
           DISPLAY 'INGRESE VALOR DEL TERCER IMPORTE:'
           ACCEPT IMPORTES-3
           DISPLAY 'INGRESE VALOR DEL CUARTO IMPORTE:'
           ACCEPT IMPORTES-4
           DISPLAY 'INGRESE VALOR DEL QUINTO IMPORTE:'
           ACCEPT IMPORTES-5.

       20000-CALCULO.
           COMPUTE TOTAL = IMPORTES-1 + IMPORTES-2 + IMPORTES-3 
                   + IMPORTES-4 + IMPORTES-5
           COMPUTE IVA = WSC-PORCENTAJE-IVA * TOTAL.

       30000-RESULTADO.
           DISPLAY 'RESULTADO: ' RESULTADO.
           

