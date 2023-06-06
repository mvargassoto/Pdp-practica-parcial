-- los objetos son funciones de Barbaro a Barbaro
-- en la parte de objetos se van a modificar las 
-- habilidades, objetos y la fuerza

-- una aventura se compone de uno o mas eventos, un 
-- evento es todo lo que reciba un barbaro y devuelva
-- si sobrevive al evento o no

-- ritualFechorias usa a sobrevivientes

-- en el punto 4 una manera puede ser una funcion recursiva
-- del estilo 
{-
habilidades x:xs
    | null x = []
    | sino = head . filter (habilidad en head) $ x:xs ++ f (xs)

esto tiene una falla:
[a,b,c,a,d,e]
en este caso "a" va a seguir estando dos veces
-}
module Barbaros where

--
-- Ejercicio 1
--

data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Float,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
}

type Objeto = Barbaro -> Barbaro
type Habilidad = String

dave = Barbaro {
    nombre= "Dave",
    fuerza = 100,
    habilidades = ["tejer", "escribirPoesia"],
    objetos = [ardilla, libroPedking]
}

libroPedking :: Objeto
libroPedking = undefined

espadas :: Float -> Objeto
espadas peso barbaro = modificarFuerza (fuerza barbaro + (2*peso)) barbaro

modificarFuerza :: Float -> Barbaro -> Barbaro
modificarFuerza cuanto barbaro = Barbaro {fuerza = cuanto}

amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos habilidad barbaro = aplicarAHabilidades ((++) [habilidad]) barbaro

aplicarAHabilidades :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro
aplicarAHabilidades operacion barbaro = Barbaro {habilidades = operacion . habilidades $ barbaro}

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro = aplicarAHabilidades ((++) ["magia"]) (Barbaro {objetos = []})


modificarObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
modificarObjetos operacion barbaro = Barbaro {objetos = operacion . objetos $ barbaro}

ardilla :: Objeto
ardilla barbaro = barbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda objetoA objetoB = objetoA . objetoB

--
-- Ejercicio 2
--

megafono :: Objeto
megafono barbaro = Barbaro {habilidades = [mayusculas . concat . habilidades $ barbaro]} 

mayusculas :: Habilidad -> Habilidad
mayusculas = undefined

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

--
-- Ejercicio 3
--

type Aventura = [Evento]
type Evento = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = contieneLaHabilidad "Escribir Poesía Atroz" barbaro
-------------------------------------------------------

cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = flip elem ["Faffy", "Astro"] . nombre $ barbaro
-------------------------------------------------------

ritualFechorias :: [Evento] -> Evento
ritualFechorias eventos barbaro = (>=1) . cuantosPasa eventos $ barbaro

cuantosPasa :: [Evento] -> Barbaro -> Int
cuantosPasa eventos barbaro = length . filter (\evento -> evento barbaro) $ eventos

saqueo :: Evento
saqueo barbaro = contieneLaHabilidad "robar" barbaro && fuerza barbaro > 80

contieneLaHabilidad :: Habilidad -> Barbaro -> Bool
contieneLaHabilidad habilidad barbaro = elem habilidad . habilidades $ barbaro

gritoDeGuerra :: Evento
gritoDeGuerra barbaro = gritoDeGuerraDeBarbaro barbaro >= cantidadLetrasHabilidades barbaro

gritoDeGuerraDeBarbaro :: Barbaro -> Int
gritoDeGuerraDeBarbaro barbaro = (*4) . length . objetos $ barbaro

cantidadLetrasHabilidades :: Barbaro -> Int
cantidadLetrasHabilidades barbaro = length . concat . habilidades $ barbaro

caligrafia :: Evento
caligrafia barbaro = all (estandarCaligrafiaPerfecta) . habilidades $ barbaro

estandarCaligrafiaPerfecta :: Habilidad -> Bool
estandarCaligrafiaPerfecta habilidad = masDeTresVocales habilidad && comienzaConMayuscula habilidad

masDeTresVocales :: Habilidad -> Bool
masDeTresVocales habilidad = (>3) . cantidadDeVocales $ habilidad

cantidadDeVocales :: String -> Int
cantidadDeVocales palabra = length . filter (esVocal) $ palabra

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

comienzaConMayuscula :: Habilidad -> Bool
comienzaConMayuscula habilidad = esMayuscula . head $ habilidad

esMayuscula :: Char -> Bool
esMayuscula = undefined
-------------------------------------------------------

sobrevivientes :: [Barbaro] -> Aventura -> Int
sobrevivientes barbaros aventura = length . filter ((== length aventura) . cuantosPasa aventura) $ barbaros

--
-- Ejercicio 4
--

sinRepetidos :: [Habilidad] -> [Habilidad]
sinRepetidos (x:xs) 
    | null xs = [x]
    | otherwise = [x] ++ sinRepetidos (filter (/= x) xs)  

-- esta es rara, no la se a priori
descendientes :: Barbaro -> [Barbaro]
descendientes barbaro = undefined
