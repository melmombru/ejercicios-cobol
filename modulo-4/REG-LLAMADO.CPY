       01  REGISTRO-LLAMADO.
           05 DATOS-RETORNO.
               10 WSV-FECHA-NUMERICA   PIC 9(08).
               10 WSV-FECHA-TEXTUAL    PIC X(20).
           05 STATUS-DATOS.
               10 WSV-STATUS-RETORNO   PIC 9(02) VALUE 00.
                   88 WSS-RETORNO-OK             VALUE 00.
                   88 WSS-RETORNO-ERROR          VALUE 11.
               10 WSV-TEXTO-STATUS     PIC X(20).
