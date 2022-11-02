      ******************************************************************
      * Author: Melanie Mombru
      * Date: 2022 11
      * DESCRIPCION: Ejercicio 0002, genera un informe de promedios de
      *los alumnos entre el mes ingresado y ese fin de año
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJ0002MM.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ENTRADA-ALUMNOS      ASSIGN TO DISK 'ALUMNOS.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS IS WSS-FS-ALUMNOS.

       SELECT ENTRADA-NOTAS        ASSIGN TO DISK 'NOTANEW.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS IS WSS-FS-NOTAS.

       SELECT SALIDA-INCIDENCIAS   ASSIGN TO DISK 'INCIDENCIAS.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS IS WSS-FS-INCIDENCIAS.

      *-----------------------
       DATA DIVISION.
       FILE SECTION.

       FD ENTRADA-ALUMNOS.
       01 REG-ALUMNOS.
           05 REG-ALU-NRO-ALUMNO               PIC 9(04).
           05 REG-ALU-NOMBRE                   PIC X(23).
           05 REG-ALU-NRO-PAIS                 PIC 9(03).

       FD ENTRADA-NOTAS.
       01 REG-NOTAS.
           05 REG-NOT-NRO-ALUMNO               PIC 9(04). 
           05 REG-NOT-NRO-MATERIA              PIC 9(02).
           05 REG-NOT-ANIO                     PIC 9(04).
           05 REG-NOT-MES                      PIC 9(02).
           05 REG-NOT-NOTA                     PIC 9(02).

       FD SALIDA-INCIDENCIAS.
       01 REG-INCIDENCIAS.
           05 REG-NRO-ALUMNO-I                 PIC 9(02).
           05 REG-TABLA-I                      PIC X(30).

      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ALUMNOS                       PIC X(02).
           88 WSS-FS-ALUMNOS-OK                        VALUE '00'.
           88 WSS-FS-ALUMNOS-EOF                       VALUE '10'.

       01 WSS-FS-NOTAS                         PIC X(02).
           88 WSS-FS-NOTAS-OK                          VALUE '00'.
           88 WSS-FS-NOTAS-EOF                         VALUE '10'.

       01 WSS-FS-INCIDENCIAS                   PIC X(02).
           88 WSS-FS-INCIDENCIAS-OK                    VALUE '00'.
           88 WSS-FS-INCIDENCIAS-EOF                   VALUE '10'.

       01 WSV-ACUM-MATERIA                     PIC 9(04).
       01 WSV-CONT-MATERIA                     PIC S9(4) COMP.

       01 WSV-CONT-ALUMNOS                     PIC S9(4) COMP.
       01 WSV-CONT-NOTAS                       PIC S9(4) COMP.
       01 WSV-CONT-INCIDENCIAS                 PIC S9(4) COMP.

       01 WSV-ALUMNOS.
           05 WSV-ALU-NRO-ALUMNO               PIC 9(04).
           05 WSV-ALU-NOMBRE                   PIC X(23).
           05 WSV-ALU-NRO-PAIS                 PIC 9(03).

       01 WSV-NOTAS.
           05 WSV-NOT-NRO-ALUMNO               PIC 9(04). 
           05 WSV-NOT-NRO-MATERIA              PIC 9(02).
           05 WSV-NOT-ANIO                     PIC 9(04).
           05 WSV-NOT-MES                      PIC 9(02).
           05 WSV-NOT-NOTA                     PIC 9(02).

      **RUTINA---------------------------------------------------------*
       01  WSC-RUTFECHA                        PIC X(08) 
                                                       VALUE "RUTFECHA".

       01  WSC-RUTPAIS                         PIC X(07) 
                                                       VALUE "RUTPAIS".

       01  WSC-RUTMAT                          PIC X(06) 
                                                       VALUE "RUTMAT".
       COPY REG-FECHA.
       COPY REG-PAIS.
       COPY REG-MAT.
       
      **INFORME--------------------------------------------------------*
       01 WSV-LINEA-FECHA.
           05 FILLER                           PIC X(64) VALUE SPACES.
           05 FILLER                           PIC X(07) VALUE "DESDE ".
       01 WSV-LINEA.
           05 FILLER                           PIC X(78) VALUE ALL "*".
       01 WSV-LINEA-ALUMNO.
           05 FILLER                           PIC X(01) VALUE "*".
           05 FILLER                           PIC X(04) VALUE SPACES.
           05 FILLER                           PIC X(09) 
                                                       VALUE "ALUMNO :".
           05 WSV-NOMBRE                       PIC X(23).
           05 FILLER                           PIC X(02) VALUE SPACES.
           05 FILLER                           PIC X(15) 
                                                       VALUE 
                                                       "NACIONALIDAD :".
           05 WSV-NACIONALIDAD                 PIC X(20).
           05 FILLER                           PIC X(03) VALUE SPACES.
           05 FILLER                           PIC X(01) VALUE "*".
       01 WSV-LINEA-MAT-PROMEDIO.
           05 FILLER                           PIC X(01) VALUE "*".
           05 FILLER                           PIC X(14) VALUE SPACES.
           05 FILLER                           PIC X(09) 
                                                       VALUE "MATERIA".
           05 FILLER                           PIC X(28) VALUE SPACES.
           05 FILLER                           PIC X(08) 
                                                       VALUE "PROMEDIO".
           05 FILLER                           PIC X(17) VALUE SPACES.
           05 FILLER                           PIC X(01) VALUE "*".
       01 WSV-LINEA-MATERIA.
           05 FILLER                           PIC X(01) VALUE "*".
           05 FILLER                           PIC X(09) VALUE SPACES.
           05 WSV-MATERIA                      PIC X(25).
           05 FILLER                           PIC X(21) VALUE SPACES.
           05 WSV-PROMEDIO                     PIC Z(02)9.99.
           05 FILLER                           PIC X(15) VALUE SPACES.
           05 FILLER                           PIC X(01) VALUE "*".
      *-----------------------
       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 10000-INICIO
           PERFORM 20000-INFORME
           PERFORM 30000-CERRAR-ARCHIVOS
           PERFORM FIN-DEL-PROGRAMA.
           
       10000-INICIO.
           PERFORM 11000-INGRESAR-FECHA
           PERFORM 12000-CARGAR-TABLAS
           PERFORM 13000-ABRIR-ARCHIVOS.

       11000-INGRESAR-FECHA.
           CALL WSC-RUTFECHA USING REGISTRO-FECHA
           PERFORM UNTIL WSS-RETORNO-OK
               CALL WSC-RUTFECHA USING REGISTRO-FECHA
           END-PERFORM.

       12000-CARGAR-TABLAS.
           MOVE "A" TO WSS-OPCION-PAIS
           CALL WSC-RUTPAIS USING REGISTRO-PAIS
           IF NOT WSS-PAIS-OK
               DISPLAY WSV-ST-TEXTO-PAIS
               DISPLAY WSV-ST-RETORNO-PAIS
           END-IF
           MOVE "A" TO WSS-OPCION-MAT
           CALL WSC-RUTMAT USING REGISTRO-MATERIA
           IF NOT WSS-MAT-OK
               DISPLAY WSV-ST-TEXTO-MAT
               DISPLAY WSV-ST-RETORNO-MAT
           END-IF.

      *ABRIR ARCHIVOS A UTILIZAR
       13000-ABRIR-ARCHIVOS.
           PERFORM 13100-ABRIR-ARCHIVO-ALUMNOS
           PERFORM 13200-ABRIR-ARCHIVO-NOTAS
           PERFORM 13300-ABRIR-ARCHIVO-INCIDENCIAS.

      *ABRIR ARCHIVO DE ALUMNOS
       13100-ABRIR-ARCHIVO-ALUMNOS.
           MOVE 0 TO WSV-CONT-ALUMNOS
           OPEN INPUT ENTRADA-ALUMNOS
           IF NOT WSS-FS-ALUMNOS-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE ALUMNOS'
               DISPLAY 'FILE STATUS ' WSS-FS-ALUMNOS
               PERFORM FIN-DEL-PROGRAMA
           END-IF.

      *ABRIR ARCHIVO DE NOTAS
       13200-ABRIR-ARCHIVO-NOTAS.
           MOVE 0 TO WSV-CONT-NOTAS
           OPEN INPUT ENTRADA-NOTAS
           IF NOT WSS-FS-NOTAS-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE NOTAS'
               DISPLAY 'FILE STATUS ' WSS-FS-NOTAS
               PERFORM FIN-DEL-PROGRAMA
           END-IF.

      *ABRIR ARCHIVO DE INCIDENCIAS
       13300-ABRIR-ARCHIVO-INCIDENCIAS.
           MOVE 0 TO WSV-CONT-INCIDENCIAS
           OPEN OUTPUT SALIDA-INCIDENCIAS
           IF NOT WSS-FS-INCIDENCIAS-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE INCIDENCIAS'
               DISPLAY 'FILE STATUS ' WSS-FS-INCIDENCIAS
               PERFORM FIN-DEL-PROGRAMA
           END-IF.

      *REALIZAR APAREO DE UNO A MUCHOS
       20000-INFORME.
           DISPLAY WSV-LINEA-FECHA WITH NO ADVANCING 
           DISPLAY WSV-FECHA-TEXTUAL
           DISPLAY WSV-LINEA
           PERFORM 21000-LEER-ARCHIVO-ALUMNOS
           PERFORM 22000-LEER-ARCHIVO-NOTAS
           PERFORM UNTIL WSS-FS-ALUMNOS-EOF AND WSS-FS-NOTAS-EOF
               IF REG-ALU-NRO-ALUMNO = REG-NOT-NRO-ALUMNO
                   MOVE REG-ALUMNOS TO WSV-ALUMNOS
                   MOVE WSV-ALU-NOMBRE TO WSV-NOMBRE
                   MOVE "B" TO WSS-OPCION-PAIS
                   MOVE WSV-ALU-NRO-PAIS TO REG-NRO-PAIS-E
                   CALL WSC-RUTPAIS USING REGISTRO-PAIS
                   MOVE REG-DESCRIP-PAIS-R TO WSV-NACIONALIDAD
                   DISPLAY WSV-LINEA-ALUMNO
                   DISPLAY WSV-LINEA-MAT-PROMEDIO
                   PERFORM 25100-CALCULAR-PROMEDIOS 
                   UNTIL ((WSS-FS-ALUMNOS-EOF AND 
                   WSS-FS-NOTAS-EOF) OR
                   WSV-ALU-NRO-ALUMNO NOT= REG-NOT-NRO-ALUMNO) 
                   PERFORM 21000-LEER-ARCHIVO-ALUMNOS
                   DISPLAY WSV-LINEA
               ELSE
                   IF REG-ALU-NRO-ALUMNO < REG-NOT-NRO-ALUMNO
                       MOVE REG-NOTAS TO WSV-ALUMNOS
                       PERFORM 24100-ESCRIBIR-INCIDENCIAS-ALUMNOS
                       PERFORM 22000-LEER-ARCHIVO-NOTAS
                   ELSE
                       PERFORM 24200-ESCRIBIR-INCIDENCIAS-NOTAS
                       PERFORM 21000-LEER-ARCHIVO-ALUMNOS
                   END-IF
               END-IF
           END-PERFORM.

      *LEER ALUMNOS, SI ES EOF LO IGUALA A HIGH VALUE
       21000-LEER-ARCHIVO-ALUMNOS.
           READ ENTRADA-ALUMNOS
           IF WSS-FS-ALUMNOS-OK
               ADD 1 TO WSV-CONT-ALUMNOS
           ELSE 
               IF NOT WSS-FS-ALUMNOS-EOF
                   MOVE HIGH-VALUE TO REG-ALUMNOS
                   DISPLAY 'ERROR EN EL READ DE ARCHIVO DE ALUMNOS'
                   DISPLAY 'FILE STATUS ' WSS-FS-ALUMNOS
               ELSE
                   MOVE HIGH-VALUE TO REG-ALUMNOS
      *            DISPLAY 'EOF ARCHIVO DE ALUMNOS'
               END-IF
           END-IF.

      *LEER CUENTA, SI ES EOF LO IGUALA A HIGH VALUE
       22000-LEER-ARCHIVO-NOTAS.
           READ ENTRADA-NOTAS
           IF WSS-FS-NOTAS-OK
               ADD 1 TO WSV-CONT-NOTAS
           ELSE 
               IF NOT WSS-FS-NOTAS-EOF
                   DISPLAY 'ERROR EN EL READ DE ARCHIVO DE NOTAS'
                   DISPLAY 'FILE STATUS ' WSS-FS-NOTAS
               ELSE
                   MOVE HIGH-VALUE TO REG-NOTAS
      *            DISPLAY 'EOF ARCHIVO DE NOTAS'
               END-IF
           END-IF.
      

      *GRABAR EL REGISTRO EN EL ARCHIVO DE INCIDENCIAS
       24000-ESCRIBIR-ARCHIVO-INCIDENCIAS.
           WRITE REG-INCIDENCIAS
           IF WSS-FS-INCIDENCIAS-OK
               ADD 1 TO WSV-CONT-INCIDENCIAS
           ELSE 
               DISPLAY 'ERROR EN EL WRITE DE ARCHIVO DE INCIDENCIAS'
               DISPLAY 'FILE STATUS ' WSS-FS-INCIDENCIAS
           END-IF.

      *ESCRIBIR EL REGISTRO QUE FALTA EN ALUMNOS 
      *EN EL ARCHIVO DE INCIDENCIAS
       24100-ESCRIBIR-INCIDENCIAS-ALUMNOS.
           MOVE REG-NOT-NRO-ALUMNO TO REG-NRO-ALUMNO-I
           MOVE 'ALUMNOS' TO REG-TABLA-I
           PERFORM 24000-ESCRIBIR-ARCHIVO-INCIDENCIAS.

      *ESCRIBIR EL REGISTRO QUE FALTA EN NOTAS 
      *EN EL ARCHIVO DE INCIDENCIAS
       24200-ESCRIBIR-INCIDENCIAS-NOTAS.
           MOVE REG-ALU-NRO-ALUMNO TO REG-NRO-ALUMNO-I
           MOVE 'NOTAS' TO REG-TABLA-I
           PERFORM 24000-ESCRIBIR-ARCHIVO-INCIDENCIAS.

      *ACTUALIZA EL SALDO DEL CLIENTE SI SU SALDO ES MAYOR O IGUAL AL
      *MONTO, SINO LO ESCRIBE EN RECHAZOS
       25100-CALCULAR-PROMEDIOS.
           MOVE REG-NOTAS TO WSV-NOTAS
           PERFORM UNTIL ((WSS-FS-ALUMNOS-EOF AND WSS-FS-NOTAS-EOF) OR
                   (WSV-ALU-NRO-ALUMNO NOT= REG-NOT-NRO-ALUMNO) OR 
                   (WSV-NOT-NRO-MATERIA NOT= REG-NOT-NRO-MATERIA)) 
                   IF REG-NOT-MES >= WSV-FECHA-NUMERICA(1:2) AND
                   REG-NOT-ANIO = WSV-FECHA-NUMERICA(3:4)
                       ADD 1 TO WSV-CONT-MATERIA
                       ADD REG-NOT-NOTA TO WSV-ACUM-MATERIA
                   END-IF
                   PERFORM 22000-LEER-ARCHIVO-NOTAS
           END-PERFORM
           COMPUTE WSV-PROMEDIO = WSV-ACUM-MATERIA / WSV-CONT-MATERIA
           MOVE "B" TO WSS-OPCION-MAT
           MOVE WSV-NOT-NRO-MATERIA TO REG-NRO-MAT-E
           CALL WSC-RUTMAT USING REGISTRO-MATERIA
           MOVE REG-DESCRIP-MAT-R TO WSV-MATERIA
           DISPLAY WSV-LINEA-MATERIA
           MOVE ZEROES TO WSV-ACUM-MATERIA
           MOVE ZEROES TO WSV-CONT-MATERIA
           MOVE ZEROES TO WSV-PROMEDIO.
       
      *CERRAR ARCHIVOS E INFORMAR
       30000-CERRAR-ARCHIVOS.
           PERFORM 31000-CERRAR-ARCHIVO-ALUMNOS
           PERFORM 32000-CERRAR-ARCHIVO-NOTAS
           PERFORM 34000-CERRAR-ARCHIVO-INCIDENCIAS.
       
      *CERRAR ARCHIVO DE ALUMNOS E INFORMAR
       31000-CERRAR-ARCHIVO-ALUMNOS.
           CLOSE ENTRADA-ALUMNOS
           IF NOT WSS-FS-ALUMNOS-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE ALUMNOS'
               DISPLAY 'FILE STATUS ' WSS-FS-ALUMNOS
           END-IF
           PERFORM 31100-INFORMAR-CANTIDAD-ALUMNOS-LEIDOS.

      *CERRAR ARCHIVO DE NOTAS E INFORMAR
       32000-CERRAR-ARCHIVO-NOTAS.
           CLOSE ENTRADA-NOTAS
           IF NOT WSS-FS-NOTAS-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE CUETAS'
               DISPLAY 'FILE STATUS ' WSS-FS-NOTAS
           END-IF
           PERFORM 32100-INFORMAR-CANTIDAD-NOTAS-LEIDAS.
       
      *CERRAR ARCHIVO DE INCIDENCIAS E INFORMAR
       34000-CERRAR-ARCHIVO-INCIDENCIAS.
           CLOSE SALIDA-INCIDENCIAS
           IF NOT WSS-FS-INCIDENCIAS-OK
               DISPLAY 'ERROR EN EL CLOSE DE ARCHIVO DE INCIDENCIAS'
               DISPLAY 'FILE STATUS ' WSS-FS-INCIDENCIAS
           END-IF
           PERFORM 34100-INFORMAR-CANTIDAD-INCIDENCIAS-ESCRITAS.

      *INFORMAR CANTIDAD DE ALUMNOS LEIDOS
       31100-INFORMAR-CANTIDAD-ALUMNOS-LEIDOS.
           IF WSV-CONT-ALUMNOS = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE ALUMNOS LEIDOS: "
               WSV-CONT-ALUMNOS
           END-IF.

      *INFORMAR CANTIDAD DE NOTAS LEIDAS
       32100-INFORMAR-CANTIDAD-NOTAS-LEIDAS.
           IF WSV-CONT-NOTAS = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE NOTAS LEIDAS: "
               WSV-CONT-NOTAS
           END-IF.

      *INFORMAR CANTIDAD DE INCIDENCIAS ESCRITOS
       34100-INFORMAR-CANTIDAD-INCIDENCIAS-ESCRITAS.
           IF WSV-CONT-INCIDENCIAS = 0
               DISPLAY "ARCHIVO VACIO"
           ELSE    
               DISPLAY "CANTIDAD DE INCIDENCIAS ESCRITAS: "
               WSV-CONT-INCIDENCIAS
           END-IF.

      *DA FIN AL PROGRAMA
       FIN-DEL-PROGRAMA.
           STOP RUN.
