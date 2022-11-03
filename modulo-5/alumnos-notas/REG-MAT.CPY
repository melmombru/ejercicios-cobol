       01  REGISTRO-MATERIA.
           05 DATOS-ENTRADA.
               10 WSS-OPCION-MAT           PIC X(01).
                   88 WSS-CARGA-MAT                VALUE "A".
                   88 WSS-BUSCAR-MAT               VALUE "B".
               10 REG-MAT-E.
                   15 REG-NRO-MAT-E        PIC 9(02).
           05 DATOS-RETORNO.
               10 REG-MAT-R.
                   15 REG-DESCRIP-MAT-R    PIC X(25).
           05 STATUS-DATOS.
               10 WSV-ST-RETORNO-MAT       PIC 9(02) VALUE 00.
                   88 WSS-MAT-OK                    VALUE 00.
                   88 WSS-MAT-ERROR                 VALUE 11.
               10 WSV-ST-TEXTO-MAT         PIC X(30).
