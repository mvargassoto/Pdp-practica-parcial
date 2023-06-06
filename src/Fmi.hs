module Fmi where

-- Ej 1 ---------------------------------------------------------

data Pais = Pais {
    ingreso :: Float,
    activaPublico :: Float,
    activaPrivado :: Float,
    recursos :: [String],
    deuda :: Float
} deriving (Show,Eq)

namibia = Pais {
    ingreso = 4140,
    activaPublico = 400000,
    activaPrivado = 650000,
    recursos = ["Minería","Ecoturismo"],
    deuda = 50000000
}

-- Ej 2 ---------------------------------------------------------

type Estrategia = Pais -> Pais

prestar :: Float -> Estrategia
prestar n pais = modificarDeuda ((+) (n*(1.5))) pais

modificarDeuda :: (Float -> Float) -> Pais -> Pais
modificarDeuda f pais = Pais {deuda = f (deuda pais)}

-----------

reducir :: Float -> Estrategia
reducir cantidad pais = aplicarEstrategias [modificarSectorPublico (-cantidad), modificarIngresoSegun ((>100) . activaPublico) (*(-0.2)) (*(-0.15))] pais

aplicarEstrategias :: [Estrategia] -> Pais -> Pais
aplicarEstrategias estrategias pais = foldl aplicarEstrategia pais estrategias

aplicarEstrategia :: Pais -> Estrategia -> Pais
aplicarEstrategia pais estrategia = estrategia pais

modificarSectorPublico :: Float -> Pais -> Pais
modificarSectorPublico delta pais = Pais {activaPublico = activaPublico pais + delta}

modificarIngresoSegun :: (Pais -> Bool) -> (Float -> Float) -> (Float -> Float) -> Pais -> Pais
modificarIngresoSegun condicion op1 op2 pais 
    | condicion pais = modificarIngreso op1 pais
    | otherwise = modificarIngreso op2 pais

modificarIngreso :: (Float -> Float) -> Pais -> Pais
modificarIngreso f pais = Pais {ingreso = f (ingreso pais)}

-----------

darle :: String -> Estrategia
darle recurso pais = aplicarEstrategias [modificarDeuda (+(-2000000)), retirarRecurso recurso] pais

-----------

retirarRecurso :: String -> Pais -> Pais
retirarRecurso recurso pais = Pais {recursos = filter (/=recurso) (recursos pais)}

-----------

blindaje :: Estrategia
blindaje pais = aplicarEstrategias [prestar (pbi pais), reducir 500] pais

pbi :: Pais -> Float
pbi pais = ingreso pais * (activaPublico pais + activaPrivado pais)

-- Ej 3 ---------------------------------------------------------
type Receta = [Estrategia]

receta = [prestar 200, darle "Minería"]

aplicarANamibia = aplicarEstrategias receta namibia

-- Ej 4 ---------------------------------------------------------

zafan :: [Pais] -> [Pais]
zafan paises = filter (elem "Petróleo" . recursos) paises

deudaTotal :: [Pais] -> Float
deudaTotal paises = sum . map (deuda) $ paises

-- filter es una funcion de orden superior: espera una función (a -> Bool) y retorna a en caso afirmativo o nada en caso contrario
-- se usa composicion en la condicion de filter y en el calculo de la deuda total
-- se usa aplicacion parcial cuando se le pasa el primer parametro a elem

-- Ej 5 ---------------------------------------------------------

ordenadaDePeorAMejor :: Pais -> [Receta] -> Bool
ordenadaDePeorAMejor pais (x:xs)
    | null xs = True
    | otherwise = pbiMenorQue x (head xs) pais && ordenadaDePeorAMejor pais xs 

pbiMenorQue :: Receta -> Receta -> Pais -> Bool
pbiMenorQue recetaA recetaB pais = (pbi . flip aplicarEstrategias pais $ recetaA) < (pbi . flip aplicarEstrategias pais $ recetaB)

-- Ej 6 ---------------------------------------------------------

{-
 si a 
    zafan :: [Pais] -> [Pais]
    zafan paises = filter (elem "Petróleo" . recursos) paises

 le llega un pais que tiene infinitos recursos aparte de "Energia", el programa jamás terminaria de 
 operar el filtrado ya que elem compara elemento a elemento, y la cantidad de elementos es infinita. 

 Con el 4b pasa lo mismo, el map va a intentar mapear a todos los elementos y si la lista es infinita 
 no termina nunca.

 El concepto que puede salvar estas situaciones es lazy evaluation, si por ejemplo a una lista
 infinita se le pide el head, no es necesario que finalice la formacion de esa lista para poder decir que 
 el primer elemento es x. 

-}


