# ejercicios-cobol

## Modulo 1 - Estructuras Basicas

Series:
* Serie 1

## Modulo 2 - Estructuras Avanzadas

* Tablas
* Registros
* Metodos de manipulacion de datos 
    - Max
    - Min 
    - Promedio
    - Ordenamiento de burbuja
    - Corte de control

Series:
 * Serie 2 (Vectores, matrices y registros)
 * Serie 3 (Metodos de manipulacion de datos)

## Modulo 3 - Archivos

Series:
 * Serie 6 (Manejo de archivos)
 * Serie 7 (Apareo)

## Modulo 4 - Rutinas y copy's

Series:
 * Serie 8 (Ejercicios integradores)

 ## Modulo 5 - Buenas Practicas

### Buenas prácticas

* Estructuras claras  en WORKING
    * Reglas de definición de datos 
    * Prefijos de nombres de datos
    * Niveles de datos 
    * Indentacion,
    * Uso de índices en tablas

* Estructuras claras  en PROCEDURE 
(un cuerpo del programa con tres PERFORM mínimo  INICIO   PROCESO   FIN)

* Indentacion de sentencias en la PROCEDURE, 

* Nombres claros de párrafos
(que indiquen claramente lo que se hace dentro del párrafo) y prefijo 
numérico de párrafos. Orden de los párrafos por su prefijo 

* PERFORM no deben ser recurrentes ( no volver de un PERFORM CON OTRO
PERFORM)
Tampoco que la cascada de PERFORM supere 4 caidas

* Nombres de variables coherentes
(que indiquen claramente que dato contienen)

* Creación de SWITCH  que clarifiquen la acción
    Niveles 88 , buen uso 

* Control de FS siempre
    TODO comando de I-O debe ser controlado, Cerrar siempre todo
archivo que fue abierto

* Párrafos únicos para apertura, cierre, lectura y escritura de datos,

* Un único STOP RUN / GOBACK / EXEC CICS RETURN.
    

#### ES MEJOR
* Comentar los párrafos 

* Un solo punto por párrafo

* PERFORM  siempre hacia párrafos que estén hacia abajo en el código

* IF acotados (que no se extiendan de una pantalla) y límites a anidamientos de IFs

* Buen uso de variables   COMP     COMP-3
            COMP  para cálculos
            COMP-3 para reducir espacio a utilizar en memoria

* Nunca grabar antes de tener seguridad de datos

* Evitar COPY en  PROCEDURE

* EVALUATE debe llevar clausula WHEN OTHER

* Usar el END-xxxxx   para IF, EVALUATE 

* Verificación de existencia de archivos o cursores definidos pero no usados

* Verificación de código muerto

* Verificar longitud de los campos que se mueven entre si (evitar
trucamientos)

#### PROHIBIDO
* Tener valores en duro en el código, no uso de hardcodes 
* En la PROCEDURE   usar campos para valores  constantes

* Verbos como GO TO y ALTER

* Comentar Sentencias
Mover campo Alfanumérico a campo numérico (Uso correcto de REDEFINES)

* DISPLAY en programa productivo 

* SORT interno 
