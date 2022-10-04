      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: 

      *Ejercicio 2     

      *Archiv  Factura  (Secuencial -Salida impresa)  

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE000V2.
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
       01  TITULO0.
           05 FILLER           PIC X(72)   VALUE SPACES.
           05 FECHA.
               10 TITUTO-FECHA PIC X(6)    VALUE 'FECHA '.
               10 WSV-DIA      PIC 9(2).
               10 FILLER       PIC X(1)   VALUE SPACES.
               10 WSV-MES      PIC 9(2).
               10 FILLER       PIC X(1)   VALUE SPACES.
               10 WSV-ANIO     PIC 9(4).
           05 FILLER           PIC X(2)   VALUE SPACES.
           05 HOJA.
               10 TITULO-HOJA  PIC X(5)   VALUE "HOJA ".
               10 NUMERO-HOJA  PIC 9(3)    VALUE 999.
       
       01  TITULO1.
           05 FILLER   PIC X(100)     VALUE ALL '*'.

       01  TITULO2.
           05 FILLER   PIC X(20)   VALUE  '* NOMBRE           *'.
           05 FILLER   PIC X(20)   VALUE  '   DIRECCION       *'.
           05 FILLER   PIC X(20)   VALUE  '   TELEFONO        *'.
           05 FILLER   PIC X(20)   
                       VALUE  '   SALDO ANTERIOR  *'.
           05 FILLER   PIC X(20)   
                       VALUE  '   SALDO ACTUAL    *'.

       01  REGIMPRE1.
           05 NOMB-CLTE      PIC X(20)   VALUE 'XXXXXXXXXXXXXXXXXX'.    
           05 FILLER         PIC X(1)   VALUE SPACES.
           05 DIRECCION      PIC X(20)   VALUE 'XXXXXXXXXXXXXXXXXX'.
           05 FILLER         PIC X(2)   VALUE SPACES.
           05 TELEFONO       PIC X(14)   VALUE "15 9 999 9999".
           05 FILLER         PIC X(7)   VALUE SPACES.
           05 SALDO-ANTERIOR PIC X(10)    VALUE '9.999.999'. 
           05 FILLER         PIC X(10)   VALUE SPACES.
           05 SALDO-ACTUAL   PIC X(10)    VALUE '9.999.999'.

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0001-MOSTRAR-FECHA
           PERFORM 0002-MOSTRAR-SEPARADOR
           PERFORM 0003-MOSTRAR-CAMPOS
           PERFORM 0002-MOSTRAR-SEPARADOR
           PERFORM 0004-MOSTRAR-REGISTRO

           STOP RUN.
           
       0001-MOSTRAR-FECHA.
           DISPLAY TITULO0.
       
       0002-MOSTRAR-SEPARADOR.
           DISPLAY TITULO1.

       0003-MOSTRAR-CAMPOS.
           DISPLAY TITULO2.

       0004-MOSTRAR-REGISTRO.
           DISPLAY REGIMPRE1.
