      ******************************************************************
      * Author: Melanie
      * Date: 2022 11
      * DESCRIPCION: Rutina pais, carga la tabla de un archivo y 
      *    luego se pueden realizar busquedas por el nro de pais
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. RUTPAIS.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT PAIS                 ASSIGN TO DISK 'nacionalidades.dat'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS IS WSS-FS-PAIS.
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD PAIS.
       01 REG-PAIS.
           05 REG-NRO-PAIS             PIC 9(03).
           05 REG-DESCRIP-PAIS         PIC X(20).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-PAIS                  PIC X(02).
           88 WSS-FS-PAIS-OK               VALUE '00'.
           88 WSS-FS-PAIS-EOF              VALUE '10'.

       01 WSV-CONT-PAIS                PIC S9(4) COMP.

       01 WST-PAISES.
           05 WST-PAIS             OCCURS 10 TIMES INDEXED BY IND.
               10 WST-NRO-PAIS         PIC 9(03).
               10 WST-DESCRIP-PAIS     PIC X(20).

       01 WSV-BUSQUEDA-PAIS            PIC X(02).
           88 WSS-BUSQUEDA-OK              VALUE 'OK'.
           88 WSS-BUSQUEDA-NO              VALUE 'NO'.

       LINKAGE SECTION.
       COPY REG-PAIS.
      *-----------------------
       PROCEDURE DIVISION USING REGISTRO-PAIS.
       00000-CONTROL.
           EVALUATE TRUE
           WHEN WSS-CARGA-PAIS
               PERFORM 10000-CARGAR-TABLA
           WHEN WSS-BUSCAR-PAIS
               PERFORM 20000-BUSCAR-PAIS
           WHEN OTHER
               SET WSS-PAIS-ERROR TO TRUE
               MOVE "ACCION SOLICITADA NO ENCONTRADA" 
               TO WSV-ST-TEXTO-PAIS
           END-EVALUATE
           PERFORM 30000-FIN-DE-RUTINA.
           
      *LEE ARCHIVO Y VA CARGANDO LOS REGISTROS A LA TABLA
       10000-CARGAR-TABLA.
           PERFORM 11000-ABRIR-ARCHIVO-INPUT
           SET IND TO 1
           PERFORM 12000-LEER-Y-CARGAR UNTIL WSS-FS-PAIS-EOF
           PERFORM 13000-CERRAR-ARCHIVO.  

      *ABRE EL ARCHIVO EN INPUT PARA LEER
       11000-ABRIR-ARCHIVO-INPUT.
           MOVE 0 TO WSV-CONT-PAIS
           OPEN INPUT PAIS
           IF WSS-FS-PAIS-OK
               SET WSS-PAIS-OK TO TRUE
           ELSE
               MOVE WSS-FS-PAIS TO WSV-ST-RETORNO-PAIS
               MOVE 'ERROR OPEN INPUT DEL ARCHIVO' 
               TO WSV-ST-TEXTO-PAIS
               PERFORM 30000-FIN-DE-RUTINA
           END-IF.  

      *LEE EL REGISTRO Y LO CARGA EN LA TABLA
       12000-LEER-Y-CARGAR.
           READ PAIS
           IF WSS-FS-PAIS-OK
               MOVE REG-PAIS TO WST-PAIS(IND)
               ADD 1 TO WSV-CONT-PAIS
               ADD 1 TO IND
           END-IF.

      *CIERRA EL ARCHIVO
       13000-CERRAR-ARCHIVO.  
           CLOSE PAIS
           IF WSS-FS-PAIS-OK
               SET WSS-PAIS-OK TO TRUE
           ELSE
               MOVE WSS-FS-PAIS TO WSV-ST-RETORNO-PAIS
               MOVE 'ERROR CLOSE DEL ARCHIVO' 
               TO WSV-ST-TEXTO-PAIS
           END-IF.

      *BUSCA EL PAIS POR SU NUMERO, VERIFICANDO QUE SEA VALIDO
       20000-BUSCAR-PAIS.
           EVALUATE TRUE
           WHEN REG-NRO-PAIS-E >= 0 AND REG-NRO-PAIS-E <= 999
               PERFORM 21000-BUSQUEDA
           WHEN OTHER
               MOVE 'NRO PAIS INVALIDO' TO WSV-ST-TEXTO-PAIS
               SET WSS-PAIS-ERROR TO TRUE
           END-EVALUATE.

      *BUSCA NRO DE PAIS Y MUEVE SU DESCRIPCION 
       21000-BUSQUEDA.
           SET WSS-BUSQUEDA-NO TO TRUE
           SET IND TO 1
           SEARCH WST-PAIS
               AT END 
                    MOVE 'NO EXISTE EL PAIS' TO WSV-ST-TEXTO-PAIS
               WHEN WST-NRO-PAIS(IND) = REG-NRO-PAIS-E
                   SET WSS-BUSQUEDA-OK TO TRUE
           END-SEARCH
           IF WSS-BUSQUEDA-OK
               MOVE WST-DESCRIP-PAIS(IND) TO REG-DESCRIP-PAIS-R
               SET WSS-PAIS-OK TO TRUE
           ELSE
               SET WSS-PAIS-ERROR TO TRUE
           END-IF.

      *DA FIN A LA RUTINA
       30000-FIN-DE-RUTINA.
           GOBACK.
           