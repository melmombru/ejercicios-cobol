      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE011.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA          ASSIGN TO DISK './CUENTAS001.DAT'
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WSS-FS-ENTRADA.
                       
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA.
       01 REG-ENTRADA.
           05 LINEA-MATRIZ     PIC X(30).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ENTRADA           PIC X(02).
           88 WSS-FS-ENTRADA-OK    VALUE '00'.
           88 WSS-FS-ENTRADA-EOF   VALUE '10'.

       01 WSV-CONT-ENTRADA     PIC S9(4) COMP.

       01  WSV-CUENTAS.
           05 FILLER               PIC X(8) VALUE 'GAS     '.
           05 FILLER               PIC X(8) VALUE 'LUZ     '.
           05 FILLER               PIC X(8) VALUE 'TELEFONO'.
           05 FILLER               PIC X(8) VALUE 'AGUA    '.

       01  WST-CUENTAS REDEFINES WSV-CUENTAS.
           05 WST-CUENTA           PIC X(8) OCCURS 4.
       
       01  WSV-MESES.
           05 FILLER               PIC X(10) VALUE 'ENERO     '.
           05 FILLER               PIC X(10) VALUE 'FEBRERO   '.
           05 FILLER               PIC X(10) VALUE 'MARZO     '.
           05 FILLER               PIC X(10) VALUE 'ABRIL     '.
           05 FILLER               PIC X(10) VALUE 'MAYO      '.
           05 FILLER               PIC X(10) VALUE 'JUNIO     '.
           05 FILLER               PIC X(10) VALUE 'JULIO     '.
           05 FILLER               PIC X(10) VALUE 'AGOSTO    '.
           05 FILLER               PIC X(10) VALUE 'SEPTIEMBRE'.
           05 FILLER               PIC X(10) VALUE 'OCTUBRE   '.
           05 FILLER               PIC X(10) VALUE 'NOVIEMBRE '.
           05 FILLER               PIC X(10) VALUE 'DICIEMBRE '.

       01  WST-MESES-NOMBRES REDEFINES WSV-MESES.
           05 WST-MES-NOMBRE       PIC X(10) OCCURS 12.

      *01 WSC-MES.
      *    05 FILLER PIC X(30) VALUE '00345-00500-00445-00090-00000-'.
      *    05 FILLER PIC X(30) VALUE '00360-00455-00440-00095-00000-'.
      *    05 FILLER PIC X(30) VALUE '00333-00521-00446-00094-00000-'.
      *    05 FILLER PIC X(30) VALUE '00300-00654-00443-00100-00000-'.
      *    05 FILLER PIC X(30) VALUE '00345-00590-00454-00089-00000-'.
      *    05 FILLER PIC X(30) VALUE '00380-00566-00490-00101-00000-'.
      *    05 FILLER PIC X(30) VALUE '00323-00600-00435-00092-00000-'.
      *    05 FILLER PIC X(30) VALUE '00299-00532-00390-00085-00000-'.
      *    05 FILLER PIC X(30) VALUE '00346-00534-00449-00090-00000-'.
      *    05 FILLER PIC X(30) VALUE '00321-00536-00545-00095-00000-'.
      *    05 FILLER PIC X(30) VALUE '00344-00569-00345-00093-00000-'.
      *    05 FILLER PIC X(30) VALUE '00380-00566-00390-00075-00000-'.
      *    05 FILLER PIC X(30) VALUE '00000-00000-00000-00000-00000-'.

       01 WST-MESES.
      *    X 1 A 13
           03 WST-MESES-DET                 OCCURS 13.
      *        Y 1 A 5
               05 WST-GASTOS-DET            OCCURS 5. 
                   10 WST-GASTOS   PIC 9(5).
                   10 FILLER       PIC X.

       01  WSV-INDICES.         
           05 WSV-I                PIC 9(02).
           05 WSV-J                PIC 9(01).

       01  WSC-DIMENSIONES.
           05  WSC-DIM-FILA        PIC 9(02) VALUE 13.
           05  WSC-DIM-COL         PIC 9(01) VALUE 5.

       77  WSV-TOTAL-COL           PIC 9(05).
       77  WSV-TOTAL-FILA          PIC 9(05).

       01  WSV-TOTALES.
           05 WSV-TOTAL-COLUMNA-5  PIC 9(06).
           05 WSV-TOTAL-FILA-13    PIC 9(06).

       01  WSS-GASTO               PIC 9(01).
           88 WSS-GASTO-MES                  VALUE 1.
           88 WSS-GASTO-CUENTA               VALUE 2.

       77  WSC-CUENTAS             PIC 9(01) VALUE 4.
       77  WSC-MESES               PIC 9(02) VALUE 12.

       01  WSV-MES                 PIC 9(02).
           88 WSS-MES                        VALUE 1 2 3 4 5 6 7 8 9 10
                                                   11 12.
       01  WSV-CUENTA              PIC 9(01).
           88 WSS-CUENTA                     VALUE 1 2 3 4.
       
       01  WSV-RECUADRO.
           05 TITULO1.
               10 FILLER           PIC X(50) VALUE ALL '*'.
           05 TITULO2.
               10 FILLER           PIC X(15) VALUE SPACES.
               10 TITULO           PIC X(4)  VALUE "MES ".
           05 TITULO3.
               10 FILLER           PIC X(15) VALUE SPACES.
               10 TITULO           PIC X(07) VALUE "CUENTA ".
           05 TITULO4.
               10 FILLER           PIC X(15) VALUE SPACES.

      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM CARGAR-ARCHIVO-A-MATRIZ
           PERFORM PROCESO-SUMAR-COLUMNAS
           PERFORM PROCESO-SUMAR-FILAS
           PERFORM PROCESO-SUMAR-TOTALES
           PERFORM PROCESO-VALIDAR-SUMAS
           PERFORM INGRESO-MENU
           PERFORM FIN-DEL-PROGRAMA.

       CARGAR-ARCHIVO-A-MATRIZ.
           PERFORM ABRIR-ARCHIVO
           PERFORM LEER-Y-CARGAR UNTIL WSS-FS-ENTRADA-EOF
           PERFORM CERRAR-ARCHIVO.

       ABRIR-ARCHIVO.
           MOVE 0 TO WSV-CONT-ENTRADA
           OPEN INPUT ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.
       
       CERRAR-ARCHIVO.
           CLOSE ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
               DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
               DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.

       LEER-Y-CARGAR.
           READ ENTRADA
           IF WSS-FS-ENTRADA-OK
               DISPLAY LINEA-MATRIZ
               ADD 1 TO WSV-CONT-ENTRADA
               MOVE LINEA-MATRIZ TO WST-MESES-DET(WSV-CONT-ENTRADA)
           END-IF.

      *PREGUNTA QUE MENU SE QUIERE MOSTRAR 
      *SE VA A REPETIR EL PROCESO HASTA QUE INGRESE UN MENU VALIDO 
      *Y LO INFORMA
       INGRESO-MENU.
           DISPLAY "INGRESE QUE GASTO QUIERE VER:"
           DISPLAY "1. POR MES"
           DISPLAY "2. POR CUENTA"
           ACCEPT WSS-GASTO
           IF WSS-GASTO-MES
               PERFORM MENU-POR-MES
           ELSE    
               IF WSS-GASTO-CUENTA 
                   PERFORM MENU-POR-CUENTA
               ELSE    
                   PERFORM INGRESO-MENU
               END-IF
           END-IF.
       
      *TOMA EL VALOR DEL MES DESEADA POR TECLADO
      *SE VA A REPETIR EL PROCESO HASTA QUE INGRESE UN MES VALIDO
       INGRESO-MES.
           DISPLAY "INGRESE MES"
           ACCEPT WSV-MES
           IF NOT WSS-MES
               PERFORM INGRESO-MES
           END-IF.

      *TOMA EL VALOR DE LA CUENTA DESEADA POR TECLADO
      *SE VA A REPETIR EL PROCESO HASTA QUE INGRESE UNA CUENTA VALIDA
       INGRESO-CUENTA.
           DISPLAY "INGRESE CUENTA"
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-CUENTAS
               DISPLAY WSV-I ". " WST-CUENTA(WSV-I)
           END-PERFORM 
           ACCEPT WSV-CUENTA
           IF NOT WSS-CUENTA
               PERFORM INGRESO-CUENTA
           END-IF.

      *MUESTRA POR PANTALLA EL INFORME DEL MES
       INFORMAR-MES.
           DISPLAY TITULO1
           DISPLAY TITULO2 WITH NO ADVANCING
           DISPLAY WST-MES-NOMBRE(WSV-MES)
           MOVE 1 TO WSV-I
           DISPLAY TITULO4 "GASTOS " WST-CUENTA(WSV-I) " = " 
               WST-GASTOS(WSV-MES,WSV-I)
           PERFORM VARYING WSV-I FROM 2 BY 1 UNTIL WSV-I > WSC-CUENTAS
               DISPLAY TITULO4 WST-CUENTA(WSV-I) " = " 
               WST-GASTOS(WSV-MES,WSV-I)
           END-PERFORM
           DISPLAY TITULO4 "TOTAL = " WST-GASTOS(WSV-MES,WSV-I)
           DISPLAY TITULO1.
       
      *MUESTRA POR PANTALLA EL INFORME DE LA CUENTA
       INFORMAR-CUENTA.
           DISPLAY TITULO1
           DISPLAY TITULO3 WITH NO ADVANCING
           DISPLAY WST-CUENTA(WSV-CUENTA)
           MOVE 1 TO WSV-I
           DISPLAY TITULO4 "MES " WST-MES-NOMBRE(WSV-I) " = " 
               WST-GASTOS(WSV-I, WSV-CUENTA)
           PERFORM VARYING WSV-I FROM 2 BY 1 UNTIL WSV-I > WSC-MESES
               DISPLAY TITULO4 WST-MES-NOMBRE(WSV-I) " = " 
               WST-GASTOS(WSV-I, WSV-CUENTA)
           END-PERFORM
           DISPLAY TITULO4 "TOTAL = "WST-GASTOS(WSV-I, WSV-CUENTA)
           DISPLAY TITULO1.

      *MUESTRA EL INFORME POR EL MES QUE SE INGRESE
       MENU-POR-MES.
           PERFORM INGRESO-MES
           PERFORM INFORMAR-MES.

      *MUESTRA EL INFORME POR LA CUENTA QUE SE INGRESE
       MENU-POR-CUENTA.
           PERFORM INGRESO-CUENTA
           PERFORM INFORMAR-CUENTA.

      *MUESTRA POR PANTALLA LA MATRIZ      
       PROCESO-MOSTRAR-MATRIZ.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL WSV-I > WSC-DIM-FILA
               PERFORM VARYING WSV-J FROM 1 BY 1 UNTIL 
               WSV-J > WSC-DIM-COL
                   DISPLAY WST-GASTOS(WSV-I, WSV-J) " " 
                   WITH NO ADVANCING 
               END-PERFORM
               DISPLAY " "
           END-PERFORM.

      *SUMA LAS COLUMNAS DEL 1 AL 4
       PROCESO-SUMAR-COLUMNAS.
           PERFORM SUMAR-TOTAL-COLUMNA VARYING WSV-J FROM 1 BY 1 UNTIL 
           WSV-J > (WSC-DIM-COL - 1).
       
      *SUMA EL TOTAL DE LA COLUMNA WSV-J Y LO GUARDA EN LA POSICION
      *(WSC-DIM-FILA, WSV-J)
       SUMAR-TOTAL-COLUMNA.
           PERFORM SUMAR-COLUMNA
           MOVE WSV-TOTAL-COL TO WST-GASTOS(WSC-DIM-FILA, WSV-J)
           MOVE 0 TO WSV-TOTAL-COL.
       
      *SUMA EL TOTAL DE LA COLUMNA WSV-J  
       SUMAR-COLUMNA.
           PERFORM VARYING WSV-I FROM 1 BY 1 UNTIL 
               WSV-I > WSC-DIM-FILA
                   COMPUTE WSV-TOTAL-COL = WSV-TOTAL-COL 
                           + WST-GASTOS(WSV-I, WSV-J)
           END-PERFORM.

      *SUMA LAS FILAS DEL 1 AL 12
       PROCESO-SUMAR-FILAS.
           PERFORM SUMAR-TOTAL-FILA VARYING WSV-I FROM 1 BY 1 UNTIL 
           WSV-I > (WSC-DIM-FILA - 1).
       
      *SUMA EL TOTAL DE LA FILA WSV-I Y LO GUARDA EN LA POSICION
      *(WSV-I, WSC-DIM-COL)
       SUMAR-TOTAL-FILA.
           PERFORM SUMAR-FILA
           MOVE WSV-TOTAL-FILA TO WST-GASTOS(WSV-I, WSC-DIM-COL)
           MOVE 0 TO WSV-TOTAL-FILA.

      *SUMA EL TOTAL DE LA FILA WSV-I
       SUMAR-FILA.
           PERFORM VARYING WSV-J FROM 1 BY 1 UNTIL 
               WSV-J > WSC-DIM-COL
                   COMPUTE WSV-TOTAL-FILA = WSV-TOTAL-FILA 
                           + WST-GASTOS(WSV-I, WSV-J)
           END-PERFORM.
      
      *SUMA LOS TOTALES DE LA FILA 13 Y DE LA COLUMNA 15
       PROCESO-SUMAR-TOTALES.
           PERFORM PROCESO-SUMAR-TOTAL-COLUMNA-5
           PERFORM PROCESO-SUMAR-TOTAL-FILA-13.

      *SUMA EL TOTAL DE LA COLUMNA 5 Y LO GUARDA EN LA VARIABLE
      *WSV-TOTAL-COLUMNA-5
       PROCESO-SUMAR-TOTAL-COLUMNA-5.
           MOVE WSC-DIM-COL TO WSV-J
           PERFORM SUMAR-COLUMNA
           MOVE WSV-TOTAL-COL TO WSV-TOTAL-COLUMNA-5.
       
      *SUMA EL TOTAL DE LA FILA 13 Y LO GUARDA EN LA VARIABLE
      *WSV-TOTAL-FILA-13
       PROCESO-SUMAR-TOTAL-FILA-13.
           MOVE WSC-DIM-FILA TO WSV-I
           PERFORM SUMAR-FILA
           MOVE WSV-TOTAL-FILA TO WSV-TOTAL-FILA-13.

      *SI EL TOTAL DE LA FILA 13 Y EL DE LA COLUMNA 5 SON IGUALES,
      *LOS GUARDA EN LA ULTIMA POSICION DE LA MATRIZ
       PROCESO-VALIDAR-SUMAS.
           IF WSV-TOTAL-COLUMNA-5 = WSV-TOTAL-FILA-13
               MOVE WSV-TOTAL-FILA-13 TO 
               WST-GASTOS(WSC-DIM-FILA, WSC-DIM-COL)
           END-IF.

       FIN-DEL-PROGRAMA.
           STOP RUN.
           