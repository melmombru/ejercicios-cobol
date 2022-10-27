      ******************************************************************
      * Author: Melanie
      * Date: 2022 10
      * DESCRIPCION: Ejercicio funcion matematica
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJES8012.
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
       01  WSV-FUNCION             PIC X(30).

       01  WSV-FORMATO-OPERACION.
           05 WSV-DATO             PIC X(05) OCCURS 5 TIMES 
                                               INDEXED BY K.

       01  WSV-DATOS-NUMERICOS.
           05 WSV-DATO-1           PIC 9(05).
           05 WSV-DATO-2           PIC 9(05).
       
       01  WSV-INDICES.         
           05 I                    PIC 9(02).
           05 J                    PIC 9(02).

       77  WSV-RESULTADO           PIC S9(05) VALUE 0.
       01  WSV-CONTADOR            PIC 9(01).
           88 WSS-PRIMER-CONTADOR      VALUE 1.
           88 WSS-SEGUNDO-CONTADOR     VALUE 2.
      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.
           PERFORM INGRESO-DE-DATOS
           PERFORM SEPARAR-FUNCION
           PERFORM REALIZAR-OPERACION
           PERFORM MOSTRAR-RESULTADOS
           PERFORM FIN-DEL-PROGRAMA.

       INGRESO-DE-DATOS.
           DISPLAY "INGRESE FUNCION: " 
           ACCEPT WSV-FUNCION.

      *BUSCAR LA PALABRA EN LA FRASE
       SEPARAR-FUNCION.
           MOVE 1 TO J
           MOVE 1 TO K
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30 
           OR WSV-FUNCION(I:1) = "="
               IF WSV-FUNCION(I:1) = " " THEN
                   MOVE WSV-FUNCION(J:I - J) TO WSV-DATO(K)
                   ADD 1 TO K
                   MOVE I TO J
                   ADD 1 TO J
               ELSE
                   CONTINUE
               END-IF
           END-PERFORM.

      *MUESTRA POR PANTALLA EL RESULTADO      
       MOSTRAR-RESULTADOS.
           DISPLAY "RESULTADO " WSV-RESULTADO.
           
        REALIZAR-OPERACION.
           SET WSS-PRIMER-CONTADOR TO TRUE
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 5
               IF WSS-PRIMER-CONTADOR
                   EVALUATE WSV-DATO(K)
                   WHEN "+"
                       MOVE WSV-DATO(K - 1) TO WSV-DATO-1
                       DISPLAY WSV-DATO-1
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = WSV-DATO-1 + WSV-DATO-2 
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE
                   WHEN "-"
                       MOVE WSV-DATO(K - 1) TO WSV-DATO-1
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = WSV-DATO-1 - WSV-DATO-2 
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE
                   WHEN "*"
                       MOVE WSV-DATO(K - 1) TO WSV-DATO-1
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = WSV-DATO-1 * WSV-DATO-2
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE   
                   WHEN "/"
                       MOVE WSV-DATO(K - 1) TO WSV-DATO-1
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = WSV-DATO-1 / WSV-DATO-2 
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE 
                   WHEN OTHER
                       CONTINUE
                   END-EVALUATE
               ELSE 
                   EVALUATE WSV-DATO(K)
                   WHEN "+"
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = 
                       WSV-RESULTADO + WSV-DATO-2
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE  
                   WHEN "-"
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = 
                       WSV-RESULTADO - WSV-DATO-2 
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE  
                   WHEN "*"
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = 
                       WSV-RESULTADO * WSV-DATO-2
                       ADD 1 TO WSV-CONTADOR   
                   WHEN "/"
                       MOVE WSV-DATO(K + 1) TO WSV-DATO-2
                       COMPUTE WSV-RESULTADO = 
                       WSV-RESULTADO / WSV-DATO-2 
                       SET  WSS-SEGUNDO-CONTADOR TO TRUE
                   WHEN OTHER
                       CONTINUE
                   END-EVALUATE
               END-IF
           END-PERFORM.

       FIN-DEL-PROGRAMA.
           STOP RUN.
         