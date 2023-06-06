module Lib where
import Text.Show.Functions
import Data.List -- para poder usar delete

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 1

data Direccion = Norte | Sur | Este | Oeste deriving (Show, Eq)
data Color = Rojo | Azul | Verde | Negro deriving (Show, Eq) 
type Coordenada = (Int, Int)

data Celda = Celda {
    posicion :: Coordenada,
    bolitas :: [Color]
} deriving (Show)

data Tablero = Tablero {
    tamanio :: Coordenada,
    cabezal :: Coordenada,
    celdas :: [Celda]
} deriving (Show)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 2

generarLista :: Int -> [Celda]
generarLista tamanio = replicate tamanio (Celda (0,0) [])

sumarTuplas :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumarTuplas (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

generarCoordenadas :: Int -> Int -> [(Int, Int)]
generarCoordenadas maxFilas maxColumnas = concat $ map (\i -> map (\j -> (i, j)) [0..(maxFilas - 1)]) [0..(maxColumnas - 1)] --Recorre la matriz y genere una lista de tuplas
                                                                                                                            -- [(0,0)(0,1)(0,2)(1,0)(1,1) ... (2,2)]                                     
-- recibe una celda con coordenada (0,0) y le suma la nueva coordenada
actualizarCoordenadaCelda :: Celda -> Coordenada -> Celda
actualizarCoordenadaCelda celda coordenada = Celda (sumarTuplas (posicion celda) coordenada) (bolitas celda)

-- recibe la lista de celdas que tienen todas (0,0) de coordenada y la lista de nuevas coordenadas, actualiza la posicion de cada celda
actualizarPosicionesLista :: [Celda] -> [Coordenada] -> [Celda]
actualizarPosicionesLista celdas coordenadasNuevas =  zipWith (actualizarCoordenadaCelda) celdas coordenadasNuevas

-- usada por el usuario, genera un tablero con el ancho y largo a pasar
generarTablero :: Int -> Int -> Tablero
generarTablero filas columnas = Tablero (filas,columnas) (0,0) (actualizarPosicionesLista (generarLista (filas * columnas)) (generarCoordenadas filas columnas))

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 3

-- usada por el usuario
mover :: Direccion -> Tablero ->  Tablero  
mover direccion tablero  
    | not (puedeMoverse (Tablero (tamanio tablero) (sumarTuplas (cabezal tablero) (movimientoEnDireccion direccion)) (celdas tablero))) = error "Cabezal se cayo del tablero"
    | otherwise = Tablero (tamanio tablero) (sumarTuplas (cabezal tablero) (movimientoEnDireccion direccion)) (celdas tablero)

puedeMoverse :: Tablero -> Bool
puedeMoverse tablero = elem (cabezal tablero) (map (\x -> posicion x) (celdas tablero))
    
movimientoEnDireccion :: Direccion -> (Int, Int)
movimientoEnDireccion Norte = (1, 0)
movimientoEnDireccion Sur = (-1, 0)
movimientoEnDireccion Este = (0, 1)
movimientoEnDireccion Oeste = (0, -1)

-- usada por el usuario
poner :: Color -> Tablero ->  Tablero
poner color tablero  = Tablero (tamanio tablero) (cabezal tablero) (map (modificarCelda (cabezal tablero) (color:) color) (celdas tablero))

modificarCelda :: Coordenada -> ([Color] -> [Color]) -> Color -> Celda -> Celda
modificarCelda  cabezal operacion color celda
    | posicion celda == cabezal = Celda (posicion celda) ((operacion) (bolitas celda))
    | otherwise = celda   

-- usada por el usuario
sacar :: Color -> Tablero ->  Tablero
sacar color tablero 
    | hay color tablero = Tablero (tamanio tablero) (cabezal tablero) (map (modificarCelda (cabezal tablero) (delete color) color) (celdas tablero))
    | otherwise = error ("No hay bolitas de color " ++ show color ++ " para sacar de la celda actual")

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 4

repetir :: Int -> [Sentencia] -> Tablero -> Tablero
repetir veces sentencias tablero = aplicarATablero tablero (concat (replicate veces sentencias))

alternativa :: (Tablero -> Bool) -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion unasSentencias otrasSentencias tablero
    | condicion tablero = aplicarATablero tablero unasSentencias
    | otherwise = aplicarATablero tablero otrasSentencias

si :: (Tablero -> Bool) -> [Sentencia] -> Tablero -> Tablero
si condicion sentencias tablero = alternativa condicion sentencias [] tablero

siNo :: (Tablero -> Bool) -> [Sentencia] -> Tablero -> Tablero
siNo condicion sentencias tablero = alternativa condicion [] sentencias tablero

mientras :: (Tablero -> Bool) -> [Sentencia] -> Tablero -> Tablero
mientras condicion sentencias tablero                              
    | condicion tablero = mientras condicion sentencias (aplicarATablero tablero sentencias)
    | otherwise = tablero   

irAlBorde :: Direccion -> Tablero -> Tablero
irAlBorde direccion tablero = mientras (puedeMoverse) [mover direccion] tablero

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 5

-- usada por el usuario
hay :: Color -> Tablero -> Bool
hay color tablero = (cantidad color tablero) > 0

-- usada por el usuario
cantidad :: Color -> Tablero -> Int 
cantidad color tablero = (length . filter (==color) . bolitas . irACeldaUbicadaEnCabezal) tablero

irACeldaUbicadaEnCabezal :: Tablero -> Celda
irACeldaUbicadaEnCabezal tablero = celdas tablero !! calcularIndiceEnCeldas (cabezal tablero) (tamanio tablero)
 
calcularIndiceEnCeldas :: (Int,Int) -> (Int,Int) -> Int
calcularIndiceEnCeldas posicion maximoTablero = fst posicion * fst maximoTablero + snd posicion

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 6

type Sentencia = Tablero -> Tablero

-- usada por el usuario
programa :: Tablero -> [Sentencia] -> Tablero
programa tablero sentencias = aplicarATablero tablero sentencias

aplicarATablero :: Tablero -> [Sentencia] -> Tablero 
aplicarATablero tablero sentencias = foldl (\acc accion -> accion acc) tablero sentencias

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 7

ejemplo = programa (generarTablero 3 3)[
    mover Norte,
    poner Negro,
    poner Negro,
    poner Azul, 
    mover Norte, 
    repetir 15 [poner Rojo, poner Azul], 
    alternativa (hay Verde) [mover Este, poner Negro] [mover Sur, mover Este, poner Azul],
    mover Este, 
    mientras ((<=9) . cantidad Verde) [poner Verde], 
    poner Azul]




