module Resumen where

-- Funciones dadas ------------------------------------------------------------------------------------

-- lenght: dada una lista devuelve su tamaño
-- genericLength: funciona IGUAL que lenght pero los diferencia que generic devuelve
-- Num y length devuelve Int (poco interesante)

-- sum: dada una lista de NUMEROS devuelve la suma de todos los elementos

-- (:): los dos puntos son un constructor de listas de todo tipo, lo que este a la izquierda 
-- va a representar el head y lo que este del lado derecho va a representar el tail

-- (++): concatena una lista "a" con otra lista "b" siendo ambas DEL MISMO TIPO

-- head: devuelve el primer elemento de cualquier lista PERO ROMPE SI ESTA VACIA
-- tail: devuelve TODO lo que no es el primer elemento PERO ROMPE SI ESTA VACIA

-- (!!): indicador de posicion para una lista, ejemplo [1,2,3] !! 1 => 2

-- sort: (necesita import Data.List) dada una lista devuelve la misma ordenada siempre y cuando
-- los elementos se sepan comparar con >,<,etc.

-- filter: dada una condicion y una lista, devuelve la lista solo con los elementos que
-- cumplen con la condicion

-- map: dada una operacion y una lista, devuelve una lista con el resultado de hacer
-- pasar a cada elemento por la operacion

-- ($): divide todo lo que es composicion del unico parametro a recibir

-- all: dada una condicion y una lista, devuelve si todos los elementos cumplen la condicion o no
-- en otras palabras es como un map con una operacion booleana y despues hace (elem1 && elem2 && .. && elemn)

-- any: dada una condicion y una lista, devuelve si algun elemento cumple la condicion
-- similar a all, lo que hace es (elem1 || elem2 || ... || elemn) despues de hacer un map booleano

-- drop: dada una cantidad y una lista, devuelve la lista con la cantidad de elementos removida 
-- contando desde el head

-- reverse: dada una lista, devuelve la lista con el orden invertido, ejemplo reverse [1,2,3] => [3,2,1]

-- null: dada una lista te devuelve si esta vacia o no

-- concat: dadas dos listas, devuelve una lista con todos los elementos

-- concatMap: dada una operacion y una lista, devuelve una lista de todos los elementos operados concatenados

-- foldl: dada una operacion, un elemento (base) y una lista, devuelve el elemento operado con 
-- cada elemento de la lista de Izquierda a Derecha

-- foldr: lo mismo que foldl pero de Derecha a Izquierda

-- dato aparte: se sabe que reverse te devuelve la lista al reves, por ende hacer (foldl . reverse) es 
-- equivalente a foldr en caso de que importe el orden de recorrido



-- Temas vistos ------------------------------------------------------------------------------------

-- expresiones lambdas: operaciones que se que voy a usar una unica vez y no vale la pena crear una funcion que lo haga

--currificacion: aplicacion parcial, en haskell todo se aplica de manera parcial en el fondo

-- guardas: condicionales segun casos

-- tuplas: vectores con acceso directo por pattern matching. En caso de ser tupla de 2 se puede usar fst snd

-- Type: etiquetas, si una una expresión se repite reiteradas veces se le puede decir a haskell
-- con etiquetas que el type creado va a representar esa expresion, es mas que nada para dar un contexto
-- que uno entiende y el lenguaje tambien

-- Data: definicion de tipo de dato estructurado

-- Pattern matching: condiciones que actuan segun el parametro llegado


-- Funciones que encontramos ------------------------------------------------------------------------

-- replicate: dada una cantidad y una lista, devuelve una LISTA DE LISTAS que contiene esa lista llegada repetida

-- zipWith: dada una operacion y dos listas, devuelve una lista con los elementos de ambas listas
-- operados de a pares, ejemplo: (+) [1,2,3] [1,2,3] => [2,4,6]

-- elem: dado un elemento y una lista, devuelve si el elemento esta
-- notElem: idem elem pero devuelve si el elemento no esta

