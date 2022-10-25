      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio encontrar palabra en frase
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES8011.
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
       01  WSV-FRASE               PIC X(80).

       01  WSV-PALABRA             PIC X(30).

       01  WSV-INDICES.         
           05 I                    PIC 9(02).
           05 J                    PIC 9(02).

       01  WSV-ENCONTRADO          PIC X(02).
           88 WSS-ENCONTRADO       VALUE "SI". 
           88 WSS-NO-ENCONTRADO    VALUE "NO".       

       77  WSV-CONTADOR            PIC 9(02) VALUE 0.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM BUSCAR-PALABRA
           PERFORM MOSTRAR-RESULTADOS
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           DISPLAY "INGRESE FRASE: " 
           ACCEPT WSV-FRASE
           DISPLAY "INGRESE PALABRA: " 
           ACCEPT WSV-PALABRA.

      *BUSCAR LA PALABRA EN LA FRASE
       BUSCAR-PALABRA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
               IF WSV-FRASE(I:1) = " " THEN
                   CONTINUE
               ELSE
                   PERFORM VARYING J FROM I BY 1 
                   UNTIL J > 80 OR WSV-FRASE(J:1) = " "
                   END-PERFORM
                   IF WSV-FRASE(I:J - I) = WSV-PALABRA
                       SET WSS-ENCONTRADO TO TRUE
                       ADD 1 TO WSV-CONTADOR
                   END-IF
                   MOVE J TO I
               END-IF
           END-PERFORM.

      *MUESTRA POR PANTALLA EL RESULTADO      
       MOSTRAR-RESULTADOS.
           IF WSS-ENCONTRADO
               DISPLAY "PALABRA ENCONTRADA " WSV-PALABRA 
               DISPLAY "CANTIDAD DE VECES EN LA FRASE: " WSV-CONTADOR
           ELSE 
               DISPLAY "PALABRA NO ENCONTRADA " WSV-PALABRA
           END-IF.
           

       FIN-DEL-PROGRAMA.
           STOP RUN.
         