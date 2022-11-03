      ******************************************************************
      * Author: Melanie
      * Date: 2022 11
      * DESCRIPCION: USOS INSPECT
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. INSPECT.
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
       77  TEXTO               PIC X(15) VALUE "PAGINA DE COBOL".
       77  CONTA               PIC 9(8).
      *-----------------------
       PROCEDURE DIVISION.
       INICIO.
      *    INSPECT TEXTO TALLYING CONTA FOR CHARACTERS
      *    INSPECT TEXTO TALLYING CONTA FOR ALL "A"
      *    INSPECT TEXTO TALLYING CONTA FOR LEADING "A"
      *    INSPECT TEXTO TALLYING CONTA FOR ALL "A" BEFORE INITIAL "N"
           INSPECT TEXTO TALLYING CONTA FOR ALL "A" AFTER INITIAL "G"
           DISPLAY CONTA
           PERFORM FIN-DEL-PROGRAMA.

       FIN-DEL-PROGRAMA.
           STOP RUN.            
