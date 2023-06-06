module Carreras where
import Data.List

-- Ejercicio 1 --------------------------------------------------------------------------------------------------------------------------------

data Color = Rojo | Blanco | Azul | Negro deriving (Show,Eq)

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
} deriving (Show,Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca autoA autoB = (color autoA /= color autoB) && ((<10) . abs $ (distancia autoA - distancia autoB))

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (not . any (estaCerca auto) $ carrera) && (all ((>) (distancia auto) . distancia) carrera)

puesto :: Auto -> Carrera -> Int
puesto auto carrera = (+1) . length . filter ((> distancia auto). distancia) $ carrera

-- Ejercicio 2 --------------------------------------------------------------------------------------------------------------------------------

corra :: Int -> Auto -> Auto
corra tiempo auto = Auto {color = color auto, velocidad = velocidad auto, distancia = distancia auto + velocidad auto * tiempo}

modificador :: (Int -> Int) -> Auto -> Auto
modificador f auto = Auto {color = color auto, velocidad = f . velocidad $ auto, distancia = distancia auto}

bajarLaVelocidad :: Int -> Int -> Int
bajarLaVelocidad cantidad velocidad
    | (velocidad - cantidad) < 0 = 0
    | otherwise = velocidad - cantidad 

-- Ejercicio 3 --------------------------------------------------------------------------------------------------------------------------------

type PowerUp = (Carrera -> Carrera)

afectarALosQueCumplen :: (Auto -> Bool) -> (Auto -> Auto) -> PowerUp
afectarALosQueCumplen criterio efecto carrera = (map efecto . filter criterio) carrera ++ filter (not . criterio) carrera

terremoto :: Auto -> PowerUp
terremoto auto carrera = afectarALosQueCumplen (estaCerca auto) (modificador (bajarLaVelocidad 50)) carrera

miguelitos :: Int -> Auto -> PowerUp
miguelitos cantidad auto carrera = afectarALosQueCumplen ((> distancia auto) . distancia) (modificador (bajarLaVelocidad cantidad)) carrera

jetPack :: Int -> Auto -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen ((==) (color auto) . color) (aplicarJet tiempo) carrera

aplicarJet :: Int -> Auto -> Auto
aplicarJet tiempo auto = Auto {color = color auto, velocidad = velocidad auto, distancia = distancia auto + velocidad auto * 2 * tiempo}

-- Ejercicio 4 --------------------------------------------------------------------------------------------------------------------------------

type Evento = (Carrera -> Carrera)
type Puesto = (Int, Color)

simularCarrera :: [Evento] -> Carrera -> [Puesto]
simularCarrera eventos carrera = puestos (foldl (\x evento -> evento x) carrera eventos)

puestos :: Carrera -> [Puesto]
puestos carrera = sortBy compararPuestos . map (\x -> (puesto x carrera, color x)) $ carrera 

compararPuestos :: Puesto -> Puesto -> Ordering
compararPuestos puestoA puestoB = compare (fst puestoA) (fst puestoB)

correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo carrera = map (corra tiempo) carrera

usarPowerUp :: (Auto -> PowerUp) -> Color -> Carrera -> Carrera
usarPowerUp powerUp colorRecibido carrera = powerUp (head . filter (\x -> (color x) == colorRecibido) $ carrera) carrera

-- Ejemplos --------------------------------------------------------------------------------------------------------------------------------

{-
Mostrar un ejemplo de uso de la función simularCarrera con autos de colores rojo, blanco, azul y
negro que vayan inicialmente a velocidad 120 y su distancia recorrida sea 0, de modo que ocurran
los siguientes eventos:
- todos los autos corren durante 30 segundos
- el azul usa el power up de jet pack por 3 segundos
- el blanco usa el power up de terremoto
- todos los autos corren durante 40 segundos
- el blanco usa el power up de miguelitos que reducen la velocidad en 20
- el negro usa el power up de jet pack por 6 segundos
- todos los autos corren durante 10 segundos
-}
listaEventos = 
    [correnTodos 30, 
    usarPowerUp (jetPack 30) Azul,
    usarPowerUp terremoto Blanco,
    correnTodos 40,
    usarPowerUp (miguelitos 20) Blanco,
    usarPowerUp (jetPack 6) Negro,
    correnTodos 10]

listaEventos2 = [usarPowerUp (jetPack 30) Azul]

autos = [
    Auto {color= Rojo, velocidad= 120 ,distancia= 0},
    Auto {color= Blanco, velocidad= 120 ,distancia= 0},
    Auto {color= Azul, velocidad= 120,distancia= 0},
    Auto {color= Negro, velocidad= 120,distancia= 0}]

{-
ghci> simularCarrera2 listaEventos autos 
[(3,Negro,7940),(1,Azul,16600),(4,Rojo,7100),(2,Blanco,9600)]
ghci> simularCarrera listaEventos autos 
[(1,Azul),(2,Blanco),(3,Negro),(4,Rojo)]
-}



















