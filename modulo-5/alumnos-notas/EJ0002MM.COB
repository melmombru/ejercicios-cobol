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

      *-----------------------
       WORKING-STORAGE SECTION.
       01 WSS-FS-ALUMNOS                       PIC X(02).
           88 WSS-FS-ALUMNOS-OK                        VALUE '00'.
           88 WSS-FS-ALUMNOS-EOF                       VALUE '10'.

       01 WSS-FS-NOTAS                         PIC X(02).
           88 WSS-FS-NOTAS-OK                          VALUE '00'.
           88 WSS-FS-NOTAS-EOF                         VALUE '10'.

       01 WSV-ACUM-MATERIA                     PIC 9(04).
       01 WSV-CONT-MATERIA                     PIC S9(4) COMP.

       01 WSV-CONT-ALUMNOS                     PIC S9(4) COMP.
       01 WSV-CONT-NOTAS                       PIC S9(4) COMP.

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
           05 FILLER                           PIC X(08) VALUE SPACES.
           05 FILLER                           PIC X(09) 
                                                       VALUE "MATERIA".
           05 FILLER                           PIC X(38) VALUE SPACES.
           05 FILLER                           PIC X(08) 
                                                       VALUE "PROMEDIO".
           05 FILLER                           PIC X(13) VALUE SPACES.
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
           PERFORM 33000-FIN-DEL-PROGRAMA.
           
      *PIDE INGRESO DE FECHA, CARGA LAS TABLAS DE MATERIAS Y PAISES, Y
      *ABRE LOS ARCHIVOS A LEER.
       10000-INICIO.
           PERFORM 11000-INGRESAR-FECHA
           PERFORM 12000-CARGAR-TABLAS
           PERFORM 13000-ABRIR-ARCHIVOS.

      *LLAMA A LA RUTINA PARA EL INGRESO DE LA FECHA
      *HASTA QUE SEA CORRECTA 
       11000-INGRESAR-FECHA.
           CALL WSC-RUTFECHA USING REGISTRO-FECHA
           PERFORM UNTIL WSS-RETORNO-OK
               CALL WSC-RUTFECHA USING REGISTRO-FECHA
           END-PERFORM.

      *LLAMA A LAS RUTINAS PARA CARGAR LAS TABLAS DE MATERIAS Y PAISES
       12000-CARGAR-TABLAS.
           PERFORM 12100-CARGAR-TABLA-MAT
           PERFORM 12200-CARGAR-TABLA-PAIS.

      *LLAMA A LA RUTINA DE MATERIAS PARA QUE CARGUE LAS MATERIAS
       12100-CARGAR-TABLA-MAT.
           MOVE "A" TO WSS-OPCION-MAT
           CALL WSC-RUTMAT USING REGISTRO-MATERIA
           IF NOT WSS-MAT-OK
               DISPLAY WSV-ST-TEXTO-MAT
               DISPLAY WSV-ST-RETORNO-MAT
           END-IF.

      *LLAMA A LA RUTINA DE PAISES PARA QUE CARGUE LOS PAISES
       12200-CARGAR-TABLA-PAIS.
           MOVE "A" TO WSS-OPCION-PAIS
           CALL WSC-RUTPAIS USING REGISTRO-PAIS
           IF NOT WSS-PAIS-OK
               DISPLAY WSV-ST-TEXTO-PAIS
               DISPLAY WSV-ST-RETORNO-PAIS
           END-IF.

      *ABRIR ARCHIVOS A UTILIZAR
       13000-ABRIR-ARCHIVOS.
           PERFORM 13100-ABRIR-ARCHIVO-ALUMNOS
           PERFORM 13200-ABRIR-ARCHIVO-NOTAS.

      *ABRIR ARCHIVO DE ALUMNOS
       13100-ABRIR-ARCHIVO-ALUMNOS.
           MOVE 0 TO WSV-CONT-ALUMNOS
           OPEN INPUT ENTRADA-ALUMNOS
           IF NOT WSS-FS-ALUMNOS-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE ALUMNOS'
               DISPLAY 'FILE STATUS ' WSS-FS-ALUMNOS
               PERFORM 33000-FIN-DEL-PROGRAMA
           END-IF.

      *ABRIR ARCHIVO DE NOTAS
       13200-ABRIR-ARCHIVO-NOTAS.
           MOVE 0 TO WSV-CONT-NOTAS
           OPEN INPUT ENTRADA-NOTAS
           IF NOT WSS-FS-NOTAS-OK
               DISPLAY 'ERROR EN EL OPEN DE ARCHIVO DE NOTAS'
               DISPLAY 'FILE STATUS ' WSS-FS-NOTAS
               PERFORM 33000-FIN-DEL-PROGRAMA
           END-IF.

      *REALIZAR APAREO DE UNO A MUCHOS
       20000-INFORME.
           PERFORM 20100-MOSTRAR-INICIO
           PERFORM 21000-LEER-ARCHIVO-ALUMNOS
           PERFORM 22000-LEER-ARCHIVO-NOTAS
           PERFORM UNTIL WSS-FS-ALUMNOS-EOF AND WSS-FS-NOTAS-EOF
               IF REG-ALU-NRO-ALUMNO = REG-NOT-NRO-ALUMNO
                   MOVE REG-ALUMNOS TO WSV-ALUMNOS
                   PERFORM 23000-MOSTRAR-PROMEDIO-ALU
                   PERFORM 21000-LEER-ARCHIVO-ALUMNOS
               ELSE
                   IF REG-ALU-NRO-ALUMNO < REG-NOT-NRO-ALUMNO
                       PERFORM 22000-LEER-ARCHIVO-NOTAS
                   ELSE
                       PERFORM 21000-LEER-ARCHIVO-ALUMNOS
                   END-IF
               END-IF
           END-PERFORM.
      
      *MUESTRA EL INICIO DEL INFORME POR PANTALLA
       20100-MOSTRAR-INICIO.
           DISPLAY WSV-LINEA-FECHA WITH NO ADVANCING 
           DISPLAY WSV-FECHA-TEXTUAL
           DISPLAY WSV-LINEA.

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
               END-IF
           END-IF.
       
      *MUESTRA INFORMACION DEL ALUMNO, Y LOS PROMEDIOS DE SUS MATERIAS
       23000-MOSTRAR-PROMEDIO-ALU.
           MOVE WSV-ALU-NOMBRE TO WSV-NOMBRE
           PERFORM 23100-BUSCAR-PAIS
           DISPLAY WSV-LINEA-ALUMNO
           DISPLAY WSV-LINEA-MAT-PROMEDIO
           DISPLAY WSV-LINEA
           PERFORM 23200-CALCULAR-PROMEDIOS 
           UNTIL ((WSS-FS-ALUMNOS-EOF AND WSS-FS-NOTAS-EOF) OR
           WSV-ALU-NRO-ALUMNO NOT= REG-NOT-NRO-ALUMNO)
           DISPLAY WSV-LINEA.
      
      *LLAMA A LA RUTINA DE PAISES, PARA OBTENER EL NOMBRE DEL PAISES
       23100-BUSCAR-PAIS.
           MOVE "B" TO WSS-OPCION-PAIS
           MOVE WSV-ALU-NRO-PAIS TO REG-NRO-PAIS-E
           CALL WSC-RUTPAIS USING REGISTRO-PAIS
           IF NOT WSS-PAIS-OK
               DISPLAY WSV-ST-TEXTO-PAIS
               DISPLAY WSV-ST-RETORNO-PAIS
           ELSE 
               MOVE REG-DESCRIP-PAIS-R TO WSV-NACIONALIDAD
           END-IF.

      *CALCULA EL PROMEDIO DE UNA MATERIA DE UN ALUMNO,
      *DESDE EL MES INGRESADO HASTA EL FIN DE ESE ANIO 
       23200-CALCULAR-PROMEDIOS.
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
           IF WSV-CONT-MATERIA NOT= 0
               PERFORM 23210-CALCULA-MOSTRAR-PROM
           END-IF.

      *CALCULA Y MUESTRA EL PROMEDIO DE LA NOTAS DE ESA MATERIA
       23210-CALCULA-MOSTRAR-PROM.
           COMPUTE WSV-PROMEDIO = WSV-ACUM-MATERIA / WSV-CONT-MATERIA
           PERFORM 23220-BUSCAR-MAT
           DISPLAY WSV-LINEA-MATERIA
           MOVE ZEROES TO WSV-ACUM-MATERIA
           MOVE ZEROES TO WSV-CONT-MATERIA
           MOVE ZEROES TO WSV-PROMEDIO.

      *LLAMA A LA RUTINA DE MATERIAS, 
      *PARA OBTENER EL NOMBRE DE LA MATERIA
       23220-BUSCAR-MAT.
           MOVE "B" TO WSS-OPCION-MAT
           MOVE WSV-NOT-NRO-MATERIA TO REG-NRO-MAT-E
           CALL WSC-RUTMAT USING REGISTRO-MATERIA
           IF NOT WSS-MAT-OK
               DISPLAY WSV-ST-TEXTO-MAT
               DISPLAY WSV-ST-RETORNO-MAT
           ELSE 
               MOVE REG-DESCRIP-MAT-R TO WSV-MATERIA
           END-IF.
       
      *CERRAR ARCHIVOS E INFORMAR
       30000-CERRAR-ARCHIVOS.
           PERFORM 31000-CERRAR-ARCHIVO-ALUMNOS
           PERFORM 32000-CERRAR-ARCHIVO-NOTAS.
       
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

      *DA FIN AL PROGRAMA
       33000-FIN-DEL-PROGRAMA.
           STOP RUN.
