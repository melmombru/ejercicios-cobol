      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: 

      *Ejercicio 2     

      *Archiv  Factura  (Secuencial -Salida impresa)  

      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE011D1.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA          ASSIGN TO DISK './GASTOSPORMES.dat'
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WSS-FS-ENTRADA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA.
       01 REG-ENTRADA.
           05 LINEA-REGISTRO       PIC X(36).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ENTRADA           PIC X(02).
           88 WSS-FS-ENTRADA-OK    VALUE '00'.
           88 WSS-FS-ENTRADA-EOF   VALUE '10'.

       01 WSV-CONT-ENTRADA         PIC S9(4) COMP.

       01 WSC-MES.
           05 FILLER PIC X(30) VALUE '00345-00500-00445-00090-00000-'.
           05 FILLER PIC X(30) VALUE '00360-00455-00440-00095-00000-'.
           05 FILLER PIC X(30) VALUE '00333-00521-00446-00094-00000-'.
           05 FILLER PIC X(30) VALUE '00300-00654-00443-00100-00000-'.
           05 FILLER PIC X(30) VALUE '00345-00590-00454-00089-00000-'.
           05 FILLER PIC X(30) VALUE '00380-00566-00490-00101-00000-'.
           05 FILLER PIC X(30) VALUE '00323-00600-00435-00092-00000-'.
           05 FILLER PIC X(30) VALUE '00299-00532-00390-00085-00000-'.
           05 FILLER PIC X(30) VALUE '00346-00534-00449-00090-00000-'.
           05 FILLER PIC X(30) VALUE '00321-00536-00545-00095-00000-'.
           05 FILLER PIC X(30) VALUE '00344-00569-00345-00093-00000-'.
           05 FILLER PIC X(30) VALUE '00380-00566-00390-00075-00000-'.
           05 FILLER PIC X(30) VALUE '00000-00000-00000-00000-00000-'.

       01 WST-MESES REDEFINES WSC-MES.
      *    X 1 A 13
           03 WST-MESES-DET        OCCURS 13.
      *        Y 1 A 5
               05 WST-GASTOS-DET   OCCURS 5. 
                   10 WST-GASTOS   PIC 9(5).
                   10 FILLER       PIC X.

       01 WST-GASTOS-POR-MES.
           05 WSV-ANIO             PIC 9(04).
           05 WSV-MES              PIC 9(02).
           05 WST-GASTOS-A         PIC X(30).

       01 WSC-MESES                PIC 9(02) VALUE 13.
       77 I                        PIC 9(02).
       77 J                        PIC 9(02).
       77 WSC-ANIOS                PIC 9(01) VALUE 5.
       77 WSC-GASTOS               PIC 9(02) VALUE 5.
       77 WSC-AUMENTO               PIC 9(02) VALUE 10.
      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM INGRESAR-ANIO
           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM 20000-ESCRITURA
           PERFORM 25000-ESCRITURA-PROXIMOS-ANIOS
           PERFORM 30000-CERRAR-ARCHIVO
           PERFORM INFORMAR-CANTIDAD-REGISTROS-LEIDOS
           PERFORM FIN-DEL-PROGRAMA.
           
       INGRESAR-ANIO.
           DISPLAY "INGRESAR ANIO A GRABAR"
           ACCEPT WSV-ANIO.

       10000-ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN OUTPUT ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
               PERFORM FIN-DEL-PROGRAMA
           END-IF.
       
       30000-CERRAR-ARCHIVO.
           CLOSE ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.

       20000-ESCRITURA.
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-MESES
           PERFORM 15000-LEER-Y-ESCRIBIR
       END-PERFORM.

       15000-LEER-Y-ESCRIBIR.
           IF I = 13
               MOVE 99 TO WSV-MES
           ELSE 
               MOVE I TO WSV-MES
           END-IF
           MOVE WST-MESES-DET(I)  TO WST-GASTOS-A
           DISPLAY REG-ENTRADA
           WRITE REG-ENTRADA FROM WST-GASTOS-POR-MES.
           IF WSS-FS-ENTRADA-OK
               ADD 1 TO WSV-CONT-ENTRADA
           END-IF.

       25000-ESCRITURA-PROXIMOS-ANIOS.
           ADD WSC-ANIOS TO WSV-ANIO
           PERFORM ACTUALIZAR-VALORES
           PERFORM 20000-ESCRITURA.

       ACTUALIZAR-VALORES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (WSC-MESES - 1)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > (WSC-GASTOS - 1)
                   ADD WSC-AUMENTO TO WST-GASTOS(I,J)
               END-PERFORM
           END-PERFORM.
       
       INFORMAR-CANTIDAD-REGISTROS-LEIDOS.
           IF WSV-CONT-ENTRADA = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ARCHIVOS LEIDOS: " WSV-CONT-ENTRADA
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
