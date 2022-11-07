      ******************************************************************
      * Author: Melanie
      * Date: 2022 11
      * DESCRIPCION:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJJUENRO.
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
       01 WSS-FS-JUEGO                       PIC X(02).
           88 WSS-FS-JUEGO-OK                        VALUE '00'.
           88 WSS-FS-JUEGO-EOF                       VALUE '10'.

       01 WSS-FS-NUM                       PIC X(02).
           88 WSS-FS-NUM-OK                        VALUE 'OK'.
           88 WSS-FS-NUM-POS                        VALUE 'PS'.
           88 WSS-FS-NUM-ER                        VALUE 'ER'.

       01 WSC-MASCARA                          PIC X(04) VALUE "XXXX".

       01 WSV-DIFICULTAD                       PIC 9(01).
            88 WSS-FACIL                            VALUE 1.
            88 WSS-INTERMEDIO                       VALUE 2.
            88 WSS-DIFICIL                          VALUE 3.

       01 WSV-INTENTOS                         PIC 9(02).

       01 WST-NUMS-INCOG.
           05 WST-NUM-INCOG                    PIC 9(01)
                                               OCCURS 4 TIMES
                                               INDEXED BY I.

       01 WST-NUMEROS.
           05 WST-NUMERO                       PIC 9(01)
                                               OCCURS 4 TIMES
                                               INDEXED BY J.
       01 WSV-NRO-ADIVINADOS.
            05 WSV-CORRECTOS                   PIC 9(01).
            05 WSV-POSICION-E                  PIC 9(01).
      **RUTINA---------------------------------------------------------*
       01  WSC-RUTAZAR                        PIC X(09)
                                                    VALUE "EJEAZAR0".

       01 AREA-COMUNICACION.
           05 CAMPO-INCOGNITO      PIC X(4).
      **SALIDA--------------------------------------------------------*
       01 WSV-LINEA-JUEGO.
            05 FILLER                          PIC X(01) VALUE "|".
            05 FILLER                          PIC X(19) VALUE SPACES.
            05 FILLER                          PIC X(12)
                                                    VALUE
                                                    "JUEGO NUMBLE".
            05 FILLER                          PIC X(19) VALUE SPACES.
            05 FILLER                          PIC X(01) VALUE "|".
       01 WSV-LINEA.
            05 FILLER                          PIC X(52) VALUE ALL "-".
       01 WSV-LINEA-DIFICULTAD.
            05 FILLER                          PIC X(05) VALUE SPACES.
            05 FILLER                          PIC X(12)
                                                    VALUE "DIFICULTAD:".
       01 WSV-LINEA-FACIL.
            05 FILLER                          PIC X(10) VALUE SPACES.
            05 FILLER                          PIC X(30)
                                                    VALUE "1. FACIL".

       01 WSV-LINEA-INTERMEDIO.
            05 FILLER                          PIC X(10) VALUE SPACES.
            05 FILLER                          PIC X(30)
                                                    VALUE
                                                    "2. INTERMEDIO".
       01 WSV-LINEA-DIFICIL.
            05 FILLER                          PIC X(10) VALUE SPACES.
            05 FILLER                          PIC X(30)
                                                    VALUE "3. DIFICIL".

      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-INICIO
           PERFORM 20000-JUEGO
           PERFORM 30000-FIN-DEL-PROGRAMA.

      *PIDE INGRESO DE FECHA, CARGA LAS TABLAS DE MATERIAS Y PAISES, Y
      *ABRE LOS ARCHIVOS A LEER.
       10000-INICIO.
           PERFORM 11000-GENERAR-NRO
           PERFORM 12000-BIENVENIDA
           PERFORM 13000-ELEGIR-DIF.

      *LLAMA A LA RUTINA PARA EL INGRESO DE LA FECHA
      *HASTA QUE SEA CORRECTA
       11000-GENERAR-NRO.
           CALL WSC-RUTAZAR USING AREA-COMUNICACION
           MOVE AREA-COMUNICACION TO WST-NUMS-INCOG
           DISPLAY WST-NUMS-INCOG.

      *LLAMA A LAS RUTINAS PARA CARGAR LAS TABLAS DE MATERIAS Y PAISES
       12000-BIENVENIDA.
           DISPLAY WSV-LINEA
           DISPLAY WSV-LINEA-JUEGO
           DISPLAY WSV-LINEA.

      *ELEGIR DIFICULTAD
       13000-ELEGIR-DIF.
           INITIALIZE WSV-INTENTOS
           DISPLAY WSV-INTENTOS
           DISPLAY WSV-LINEA-DIFICULTAD
           DISPLAY WSV-LINEA-FACIL
           DISPLAY WSV-LINEA-INTERMEDIO
           DISPLAY WSV-LINEA-DIFICIL
           SET WSS-FACIL TO TRUE.
      *    ACCEPT WSV-DIFICULTAD.

      *REALIZAR APAREO DE UNO A MUCHOS
       20000-JUEGO.
           PERFORM UNTIL WSV-INTENTOS >= 5 OR WSS-FS-JUEGO-OK
               PERFORM 21000-PERDIR-NROS
               MOVE ZEROES TO WSV-NRO-ADIVINADOS
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                 SET WSS-FS-NUM-ER TO TRUE
                 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4 
                 OR WSS-FS-NUM-OK
                      IF WST-NUM-INCOG(I) = WST-NUMERO(J) 
                      AND WSS-FS-NUM-ER
                           IF I = J
                                ADD 1 TO WSV-CORRECTOS
                                SET WSS-FS-NUM-OK TO TRUE
                                IF WSS-FS-NUM-POS
                                     COMPUTE WSV-POSICION-E = 
                                     WSV-POSICION-E - 1
                                END-IF
                           ELSE
                                ADD 1 TO WSV-POSICION-E
                                SET WSS-FS-NUM-POS TO TRUE
                           END-IF
                      END-IF
                 END-PERFORM
               END-PERFORM
               IF WSV-CORRECTOS NOT= 4
                 PERFORM 22000-MOSTRAR-RESULTADOS
                 ADD 1 TO WSV-INTENTOS
               ELSE
                 SET WSS-FS-JUEGO-OK TO TRUE
               END-IF
           END-PERFORM
           IF WSS-FS-JUEGO-OK
              DISPLAY "GANO, LA SECUENCIA ERA: " WST-NUMS-INCOG
           ELSE
              DISPLAY "PERDIO, LA SECUENCIA ERA: " WST-NUMS-INCOG
           END-IF.

       21000-PERDIR-NROS.
           DISPLAY "INGRESE NUMEROS:"
           ACCEPT WST-NUMEROS.

       22000-MOSTRAR-RESULTADOS.
           DISPLAY "NUMEROS CORRECTOS: " WSV-CORRECTOS
           DISPLAY "NUMEROS EN POSICION INCORRECTA: " WSV-POSICION-E.

      *DA FIN AL PROGRAMA
       30000-FIN-DEL-PROGRAMA.
           STOP RUN.
