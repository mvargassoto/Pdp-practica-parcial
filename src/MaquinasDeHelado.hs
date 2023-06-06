module MaquinasDeHelado where
import Data.List

-- un helado tiene: nombre, 
-- temperatura y 
-- proporcion de agua

type Helado = (String,Integer,Float)

salioMal :: Helado -> Bool
salioMal (nombre, temperatura, proporcion) = temperatura > 0 && proporcionAguaCorrecta nombre proporcion

proporcionAguaCorrecta :: String -> Float -> Bool
proporcionAguaCorrecta "frutilla" proporcion = proporcion == 0.4
proporcionAguaCorrecta "durazno" proporcion = elem proporcion [0.2 .. 0.6]
proporcionAguaCorrecta nombre proporcion 
    | (<=8) . length $ nombre = proporcion == (genericLength nombre) / 10
    | otherwise = proporcion == (genericLength . filter (\x -> elem x ['a','e','i','o','u']) $ nombre) / 10

frutilla = ("frutilla",12,0.4)
frutilla2 = ("frutilla",0,0.6)
durazno = ("durazno",0,0.4)
uva = ("uva",0,0.4)
mandarina = ("mandarina",0,0.4)

heladera :: Helado -> Int -> Helado
heladera (nombre,grados,proporcion) temperatura = (nombre,grados - temperatura,proporcion)

batidora :: String -> Int -> Float -> Helado
batidora fruta temperaturaAgua proporcion = (fruta,temperatura,proporcion)

-- aca consultaria que tiene que ver el peso, a priori no tiene sentido modelarlo
exprimidora :: String -> String
exprimidora = undefinded

mixturadora :: Helado -> Helado -> Helado
mixturadora (nombreA,tempA,proporA) (nombreB,tempB,proporB) = (nombreA ++ "-" ++ nombreB, min tempA tempB, average [proporA,proporB])

dispenser :: Int -> (Int,Int)
dispenser cantidad = (cantidad,0)


-- esta parte esta medio confusa, se podria modelar el type Maquina y asi poder hacer la funcion de composicion pero 
-- no todas tienen la misma salida..
