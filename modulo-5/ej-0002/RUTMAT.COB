      ******************************************************************
      * Author: Melanie
      * Date: 2022 11
      * DESCRIPCION: Rutina materia, carga la tabla de un archivo y 
      *    luego se pueden realizar busquedas por el nro de materia
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. RUTMAT.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT MATERIA                  ASSIGN TO DISK 'materias.dat'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-MATERIA.
      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD MATERIA.
       01 REG-MATERIA.
           05 REG-NRO-MATERIA              PIC 9(02).
           05 REG-DESCRIP-MATERIA          PIC X(25).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-MATERIA                   PIC X(02).
           88 WSS-FS-MATERIA-OK                VALUE '00'.
           88 WSS-FS-MATERIA-EOF               VALUE '10'.

       01 WSV-CONT-MATERIA                 PIC S9(4) COMP.

       01 WST-MATERIAS.
           05 WST-MATERIA             OCCURS 10 TIMES INDEXED BY IND.
               10 WST-NRO-MATERIA          PIC 9(02).
               10 WST-DESCRIP-MATERIA      PIC X(25).

       01 WSV-BUSQUEDA-MAT                 PIC X(02).
           88 WSS-BUSQUEDA-OK                  VALUE 'OK'.
           88 WSS-BUSQUEDA-NO                  VALUE 'NO'.

       LINKAGE SECTION.
       COPY REG-MAT.
      *-----------------------
       PROCEDURE DIVISION USING REGISTRO-MATERIA.
       00000-CONTROL.
           EVALUATE TRUE
           WHEN WSS-CARGA-MAT
               PERFORM 10000-CARGAR-TABLA
           WHEN WSS-BUSCAR-MAT
               PERFORM 20000-BUSCAR-MATERIA
           WHEN OTHER
               SET WSS-MAT-ERROR TO TRUE
               MOVE "ACCION SOLICITADA NO ENCONTRADA" 
               TO WSV-ST-TEXTO-MAT
           END-EVALUATE
           PERFORM 30000-FIN-DE-RUTINA.
           

       10000-CARGAR-TABLA.
           PERFORM 11000-ABRIR-ARCHIVO-INPUT
           SET IND TO 1
           PERFORM 12000-LEER-Y-CARGAR UNTIL WSS-FS-MATERIA-EOF
           PERFORM 13000-CERRAR-ARCHIVO.  

       11000-ABRIR-ARCHIVO-INPUT.
           MOVE 0 TO WSV-CONT-MATERIA
           OPEN INPUT MATERIA
           IF WSS-FS-MATERIA-OK
               SET WSS-MAT-OK TO TRUE
           ELSE
               MOVE WSS-FS-MATERIA TO WSV-ST-RETORNO-MAT
               MOVE 'ERROR OPEN INPUT DEL ARCHIVO' 
               TO WSV-ST-TEXTO-MAT
               PERFORM 30000-FIN-DE-RUTINA
           END-IF.  

       12000-LEER-Y-CARGAR.
           READ MATERIA
           IF WSS-FS-MATERIA-OK
               MOVE REG-MATERIA TO WST-MATERIA(IND)
               ADD 1 TO WSV-CONT-MATERIA
               ADD 1 TO IND
           END-IF.

       13000-CERRAR-ARCHIVO.  
           CLOSE MATERIA
           IF WSS-FS-MATERIA-OK
               SET WSS-MAT-OK TO TRUE
           ELSE
               MOVE WSS-FS-MATERIA TO WSV-ST-RETORNO-MAT
               MOVE 'ERROR CLOSE DEL ARCHIVO' 
               TO WSV-ST-TEXTO-MAT
           END-IF.

       20000-BUSCAR-MATERIA.
           EVALUATE TRUE
           WHEN REG-NRO-MAT-E >= 0 AND REG-NRO-MAT-E <= 99
               PERFORM 21000-BUSQUEDA
           WHEN OTHER
               MOVE 'NRO MATERIA INVALIDO' TO WSV-ST-TEXTO-MAT
               SET WSS-MAT-ERROR TO TRUE
           END-EVALUATE.

       21000-BUSQUEDA.
           SET WSS-BUSQUEDA-NO TO TRUE
           SET IND TO 1
           SEARCH WST-MATERIA
               AT END 
                    MOVE 'NO EXISTE LA MATERIA' TO WSV-ST-TEXTO-MAT
               WHEN WST-NRO-MATERIA(IND) = REG-NRO-MAT-E
                   SET WSS-BUSQUEDA-OK TO TRUE
           END-SEARCH
           IF WSS-BUSQUEDA-OK
               MOVE WST-DESCRIP-MATERIA(IND) TO REG-DESCRIP-MAT-R
               SET WSS-MAT-OK TO TRUE
           ELSE
               SET WSS-MAT-ERROR TO TRUE
           END-IF.

       30000-FIN-DE-RUTINA.
           GOBACK.
           