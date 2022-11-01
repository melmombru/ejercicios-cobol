       01  REGISTRO-PAIS.
           05 DATOS-ENTRADA.
               10 WSS-OPCION-PAIS           PIC X(01).
                   88 WSS-CARGA-PAIS                VALUE "A".
                   88 WSS-BUSCAR-PAIS               VALUE "B".
               10 REG-PAIS-E.
                   15 REG-NRO-PAIS-E        PIC 9(03).
           05 DATOS-RETORNO.
               10 REG-PAIS-R.
                   15 REG-DESCRIP-PAIS-R    PIC X(20).
           05 STATUS-DATOS.
               10 WSV-ST-RETORNO-PAIS       PIC 9(02) VALUE 00.
                   88 WSS-PAIS-OK                    VALUE 00.
                   88 WSS-PAIS-ERROR                 VALUE 11.
               10 WSV-ST-TEXTO-PAIS         PIC X(30).
