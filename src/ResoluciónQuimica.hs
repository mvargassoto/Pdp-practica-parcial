-------------
-- Punto 1 --

data Sustancia
  = Elemento {
      nombre :: String,
      simbolo :: String,
      numeroAtomico :: Int,
      grupo :: Grupo
    } 
  | Compuesto {
      nombre :: String,
      componentes :: [Componente],
      grupo :: Grupo
    }
  deriving (Show)

data Componente = Componente {
  sustancia :: Sustancia,
  cantidad :: Int
} deriving (Show)

data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

hidrogeno :: Sustancia
hidrogeno = Elemento "Hidrogeno" "H" 1 NoMetal

oxigeno :: Sustancia
oxigeno = Elemento "Oxigeno" "O" 8 NoMetal

agua :: Sustancia
agua = Compuesto "Agua" [Componente hidrogeno 2, Componente oxigeno 1] NoMetal


-------------
-- Punto 2 --

type Criterio = Sustancia -> Bool

conduceBien :: Criterio -> Sustancia -> Bool
conduceBien criterio sustancia = esMetal sustancia || criterio sustancia

electricidad :: Criterio
electricidad (Elemento _ _ _ GasNoble) = True
electricidad _                         = False

calor :: Criterio
calor (Compuesto _ _ Halogeno) = True
calor _ = False

esMetal :: Criterio
esMetal sustancia = grupo sustancia == Metal


-------------
-- Punto 3 --

nombreDeUnion :: String -> String
nombreDeUnion unNombre
  | (not . terminaEnVocal) unNombre = unNombre ++ "uro"
  | otherwise                       = sinUltimasVocales unNombre ++ "uro"

terminaEnVocal :: String -> Bool
terminaEnVocal unaPalabra = esVocal (last unaPalabra)

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

sinUltimasVocales :: String -> String
sinUltimasVocales = reverse . dropWhile esVocal . reverse


-------------
-- Punto 4 --

combinar :: String -> String -> String
combinar nombre1 nombre2 = nombreDeUnion nombre1 ++ " de " ++ nombre2


-------------
-- Punto 5 --

mezclar :: Componente -> Componente -> Sustancia
mezclar componente1 componente2 = 
  Compuesto {
    nombre = nombreCompuestoPor componente1 componente2,
    componentes = [componente1, componente2],
    grupo = NoMetal
  }

nombreCompuestoPor :: Componente -> Componente -> String  
nombreCompuestoPor componente1 componente2 =
  combinar (nombreSustancia componente1) (nombreSustancia componente2)

nombreSustancia :: Componente -> String
nombreSustancia = nombre . sustancia


-------------
-- Punto 6 --

formula :: Sustancia -> String
formula (Elemento _ simbolo _ _) = simbolo
formula (Compuesto _ componentes _) = "(" ++ concatMap formulaComponente componentes ++ ")"

formulaComponente :: Componente -> String
formulaComponente (Componente sustancia cantidad)
  | cantidad > 1 = formula sustancia ++ show cantidad
  | otherwise    = formula sustancia