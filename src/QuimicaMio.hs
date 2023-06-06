module QuimicaMio where

data Sustancia = 
    Elemento {
        nombre :: String,
        simbolo :: String,
        numeroAtomico :: Int,
        especie :: Especie
    }
    |
    Compuesto {
        nombre :: String,
        componentes :: [Componente],
        especie :: Especie
    } deriving (Show)

type Componente = (Sustancia, Int)

data Especie = NoMetal | Metal | Halogeno | Gas deriving (Show,Eq)

data Criterio = Calor | Electricidad deriving (Show,Eq)

-- ejercicio 1 ---------------------------

hidrogeno :: Sustancia
hidrogeno = Elemento {nombre = "Hidrogeno", simbolo = "H", numeroAtomico = 1, especie = NoMetal}

oxigeno :: Sustancia
oxigeno = Elemento {nombre = "Oxigeno", simbolo = "O", numeroAtomico = 8, especie = NoMetal}

agua :: Sustancia
agua = Compuesto {nombre = "Agua", componentes = [(hidrogeno,2),(oxigeno,1)], especie = NoMetal}

-- ejercicio 2 ---------------------------

conduceBien :: Sustancia -> Criterio -> Bool
conduceBien sustancia criterio = relacionConductividad (especie sustancia) criterio

relacionConductividad :: Especie -> Criterio -> Bool
relacionConductividad Metal Calor = True
relacionConductividad Gas Electricidad = True
relacionConductividad Halogeno Calor = True
relacionConductividad _ _ = False

-- ejercicio 3 ---------------------------

nombreDeUnion :: Sustancia -> String
nombreDeUnion sustancia 
    | esVocal . head . reverse . nombre $ sustancia = hastaUltimaConsonante (nombre sustancia) ++ "uro"
    | otherwise = nombre sustancia ++ "uro"

esVocal :: Char -> Bool
esVocal letra = elem letra ['a','e','i','o','u']

hastaUltimaConsonante :: String -> String
hastaUltimaConsonante nombre 
    | esVocal . head . reverse $ nombre = hastaUltimaConsonante (init nombre)
    | otherwise = nombre

-- ejercicio 4 ---------------------------

combinar :: Sustancia -> Sustancia -> String
combinar sustanciaA sustanciaB = nombreDeUnion sustanciaA ++ " de " ++ nombre sustanciaB

cloro = Elemento {nombre = "Cloro", simbolo = "Cl", numeroAtomico = 17, especie = NoMetal}

sodio = Elemento {nombre = "Sodio", simbolo = "Na", numeroAtomico = 11, especie = Metal}

-- ejercicio 5 ---------------------------

mezclar :: [Componente] -> Sustancia
mezclar componentes = Compuesto {nombre = combinarNombres componentes, componentes = componentes, especie = NoMetal}

combinarNombres :: [Componente] -> String
combinarNombres componentes
    | (>1) . length $ componentes = (nombreDeUnion . fst . head $ componentes ) ++ " de " ++ (combinarNombres . drop 1 $ componentes)
    | otherwise = nombre . fst . head $ componentes

-- ejercicio 6 ---------------------------

formula :: Sustancia -> String
formula (Elemento _ simbolo _ _ )= simbolo 
formula (Compuesto _ componentes _) = concatMap representacionDeComponente componentes

representacionDeComponente :: Componente -> String
representacionDeComponente componente 
    | (==1) . snd $ componente = nombre . fst $ componente
    | otherwise = (nombre . fst $ componente) ++ show (snd componente)





