       01  REGISTRO-FECHA.
           05 DATOS-RETORNO.
               10 WSV-FECHA-NUMERICA   PIC 9(06).
               10 WSV-FECHA-TEXTUAL    PIC X(10).
           05 STATUS-DATOS.
               10 WSV-STATUS-RETORNO   PIC 9(02) VALUE 00.
                   88 WSS-RETORNO-OK             VALUE 00.
                   88 WSS-RETORNO-ERROR          VALUE 11.
               10 WSV-TEXTO-STATUS     PIC X(20).
