      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES303.
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
       01  WST-NOTAS.
           05 WST-ALUMNOS          OCCURS 10 TIMES.
               10 WST-LEGAJO           PIC 9(02).
               10 WST-NOTA             PIC 9(02).

       01  WSV-INDICES.         
           05 I                    PIC 9(02).
           05 J                    PIC 9(02).

       77  WSV-LEGAJO-ANTERIOR     PIC 9(02).

       77  WSC-TOTAL-ALUMNOS       PIC 9(02) VALUE 10.

       77  WSV-PROMEDIO            PIC 9(03)V9(02). 
       77  WSV-CONTADOR-NOTAS      PIC 9(02).
       77  WSV-ACUMULADO           PIC 9(05).

       77  WSV-AUXILIAR            PIC 9(04).
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM ORDENAMIENTO-BURBUJA
           PERFORM CORTE-DE-CONTROL
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-TOTAL-ALUMNOS
               DISPLAY "INGRESE NUMERO DE LEGAJO " 
               ACCEPT WST-LEGAJO(I)
               DISPLAY "INGRESE NOTA DEL ALUMNO " 
               ACCEPT WST-NOTA(I)
               DISPLAY WST-ALUMNOS(I)
           END-PERFORM. 

      *ORDENA DE FORMA ASCENDENTE
       ORDENAMIENTO-BURBUJA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WSC-TOTAL-ALUMNOS
               PERFORM VARYING J FROM 1 BY 1 
               UNTIL J > WSC-TOTAL-ALUMNOS - I
                   IF WST-LEGAJO(J) > WST-LEGAJO(J + 1)
                       MOVE WST-ALUMNOS(J) TO WSV-AUXILIAR
                       MOVE WST-ALUMNOS(J + 1) TO WST-ALUMNOS(J)
                       MOVE WSV-AUXILIAR TO WST-ALUMNOS(J + 1)
                   END-IF
               END-PERFORM
               DISPLAY WST-NOTAS
           END-PERFORM.

      *MUESTRA POR PANTALLA EL PROMEDIO DEL ALUMNO      
       MOSTRAR-PROMEDIO.
           COMPUTE WSV-PROMEDIO = WSV-ACUMULADO / WSV-CONTADOR-NOTAS
           DISPLAY "PROMEDIO " WSV-PROMEDIO
           DISPLAY " ".

      *CORTE DE CONTROL POR LEGAJO DEL ALUMNO
       CORTE-DE-CONTROL.
           MOVE 1 TO I
           MOVE WST-LEGAJO(I) TO WSV-LEGAJO-ANTERIOR
           PERFORM UNTIL I > WSC-TOTAL-ALUMNOS
               MOVE ZEROES TO WSV-ACUMULADO
               MOVE ZEROES TO WSV-CONTADOR-NOTAS
               PERFORM UNTIL ((I > WSC-TOTAL-ALUMNOS) OR 
               (WST-LEGAJO(I) NOT = WSV-LEGAJO-ANTERIOR))
                   COMPUTE WSV-ACUMULADO = WST-NOTA(I) + 
                   WSV-ACUMULADO 
                   ADD 1 TO WSV-CONTADOR-NOTAS
                   ADD 1 TO I
               END-PERFORM
               DISPLAY "LEGAJO ALUMNO: " WSV-LEGAJO-ANTERIOR
               PERFORM MOSTRAR-PROMEDIO
               MOVE WST-LEGAJO(I) TO WSV-LEGAJO-ANTERIOR
           END-PERFORM.
           
       FIN-DEL-PROGRAMA.
           STOP RUN.
         