       01 REGISTRO-ARCHIVO.
           05 DATOS-ENTRADA.
               10 WSS-OPCION           PIC X(3).
                   88 WSS-OIN          VALUE "OIN".
                   88 WSS-OOU          VALUE "OOU".
                   88 WSS-OIO          VALUE "OIO".
                   88 WSS-CLO          VALUE "CLO".
                   88 WSS-REA          VALUE "REA".
                   88 WSS-WRI          VALUE "WRI".
               10 REG-ARCH-E.
                   15 REG-CLAVE-E      PIC X(2).
                   15 REG-NOMBRE-E     PIC X(30).
                   15 REG-FECHA-E      PIC X(8).
           05 DATOS-RETORNO.
               10 REG-ARCH-R.
                   15 REG-CLAVE-R      PIC X(2).
                   15 REG-NOMBRE-R     PIC X(30).
                   15 REG-FECHA-R      PIC X(8).
           05 STATUS-DATOS.
               10 WSV-ST-RETORNO       PIC 9(02) VALUE 00.
                   88 WSS-ST-OK                  VALUE 00.
                   88 WSS-ST-ERROR               VALUE 11.
               10 WSV-ST-TEXTO         PIC X(30).
               