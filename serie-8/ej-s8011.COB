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

       01  WSV-REEMPLAZO           PIC X(30).

       01  WSV-FRASE-N             PIC X(80).

       01  WSV-LONGITUDES.
           05 LONG-PALABRA         PIC 9(02).
           05 LONG-REEMPLAZO       PIC 9(02).
       
       01  WSV-INDICES.         
           05 I                    PIC 9(02).
           05 J                    PIC 9(02).
           05 K                    PIC 9(02) VALUE 0.

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
                   MOVE I TO J
                   PERFORM UNTIL J > 80 OR WSV-FRASE(J:1) = " "
                       ADD 1 TO J 
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
               PERFORM REEMPLAZO
               DISPLAY "FRASE REEMPLAZADA: "
               DISPLAY WSV-FRASE-N
           ELSE 
               DISPLAY "PALABRA NO ENCONTRADA " WSV-PALABRA
           END-IF.
           
       REEMPLAZO.
           DISPLAY "INGRESE PALABRA DE REEMPLAZO: "
           ACCEPT WSV-REEMPLAZO
           PERFORM DESCUBRIR-LONGITUD
           ADD 1 TO K
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
               IF WSV-FRASE(I:1) = " " THEN
                   CONTINUE
               ELSE
                   MOVE I TO J
                   PERFORM UNTIL J > 80 OR WSV-FRASE(J:1) = " "
                       ADD 1 TO J 
                   END-PERFORM 
                   IF WSV-FRASE(I:J - I) = WSV-PALABRA
                       MOVE WSV-REEMPLAZO TO 
                       WSV-FRASE-N(K:LONG-REEMPLAZO)
                       COMPUTE K = K + LONG-REEMPLAZO + 1
                   ELSE 
                       MOVE WSV-FRASE(I:J) TO WSV-FRASE-N(K:(J - I))
                       COMPUTE K = K + J - I + 1
                   END-IF
                   MOVE J TO I
               END-IF
           END-PERFORM.

       DESCUBRIR-LONGITUD.
           MOVE 1 TO I
           PERFORM UNTIL I > 30 OR WSV-REEMPLAZO(I:1) = " "
               ADD 1 TO I
           END-PERFORM
           COMPUTE I = I - 1
           MOVE I TO LONG-REEMPLAZO.

       FIN-DEL-PROGRAMA.
           STOP RUN.
         