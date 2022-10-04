      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio 1
      *    Tenemos estos archivos:

      *    ARCHIVO CLIENTES   (Indice por Nro cliente)
      *    Registro           NIVEL 01  REGCLTE
      *    Nro de cliente        numerico de 4
      *    Nombre        alfa numerico de 30
      *    Dirección        alfa numerico de 20
      *    Teléfono        alfa numerico de 15

      *    ARCHIVO     COMPRA    (Secuencial)
      *    Registro  NIVEL 01          REGCOMPRA
      *    Nro cliente        numerico de 4
      *    Nro producto        numerico  de 5
      *    Cantidad        numérico de 6

      *    Archivo PRODUCTO   (Indice por Nro Producto)
      *    REGISTROP   NIVEL 01        REGPDCTO
      *    Nro Producto        numérico de 4
      *    Nombre producto    alfa numérico de 20
      *    Valor x unidad        numérico  de 6, con 2 decimales

      *    DEFINIR los registros anteriores  con niveles 01 y 05  
      *    los PIC correspondientes
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE000V1.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------
       DATA DIVISION.
       FILE SECTION.

      *-----------------------
       WORKING-STORAGE SECTION.
      *    ARCHIVO CLIENTES   (Indice por Nro cliente)
      *    Registro           NIVEL 01  REGCLTE
      *    Nro de cliente        numerico de 4
      *    Nombre        alfa numerico de 30
      *    Dirección        alfa numerico de 20
      *    Teléfono        alfa numerico de 15

       01  WSV-REGCLTE.
           05 WSV-NRO-CLTE     PIC 9(4).
           05 WSV-NOMBRE       PIC X(30).
           05 WSV-DIRECCION    PIC X(20).
           05 WSV-TELEFONO     PIC X(15).
      *    ARCHIVO     COMPRA    (Secuencial)
      *    Registro  NIVEL 01          REGCOMPRA
      *    Nro cliente        numerico de 4
      *    Nro producto        numerico  de 5
      *    Cantidad        numérico de 6
       
       02  WSV-COMPRA.

      *    Archivo PRODUCTO   (Indice por Nro Producto)
      *    REGISTROP   NIVEL 01        REGPDCTO
      *    Nro Producto        numérico de 4
      *    Nombre producto    alfa numérico de 20
      *    Valor x unidad        numérico  de 6, con 2 decimales

      *-----------------------
       PROCEDURE DIVISION.

       0000-CUERPO-DEL-PROG.

           PERFORM 1000-ENTRAR-ALUMNO

           PERFORM 2000-MOSTRAR

           STOP RUN.

       1000-ENTRAR-ALUMNO.
           DISPLAY 'QUIEN VA A ESTUDIAR ? '
           ACCEPT  WSV-ALUMNO .
       2000-MOSTRAR.
           DISPLAY  '                  Bienvenido al curso ' WSV-ALUMNO.
