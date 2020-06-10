module Lib where
import Text.Show.Functions

laVerdad = True

data Festival = Festival {
    lugarDondeSeRealiza :: String,
    cantidadPublico :: Int,
    estadoDeAnimo :: EstadoDeAnimo,
    bandasQueTocan :: [Banda]
} deriving (Show)

hullabalooza :: Festival
hullabalooza = Festival "Springfield" 20000 "indiferente" [miranda, losRedondos, metalica, soda]

data Banda = Banda {
    nombreBanda :: String,
    descripcion :: [String],
    decibelesQueToca :: Int,
    genero :: Genero
} deriving Show

type Genero = Festival -> Festival
type EstadoDeAnimo = String

--BANDAS

losRedondos :: Banda
losRedondos = Banda "Los Redondos" ["legendaria", "pogosa"] 45 rockNacional

soda :: Banda
soda = Banda "Soda" ["irrepetible"] 40 rockNacional

miranda :: Banda
miranda = Banda "Miranda" ["insipida", "incolora", "inodora"] 60 pop

metalica :: Banda
metalica = Banda "Metalica" ["legendaria", "vendida"] 60 heavyMetal 

slayer :: Banda
slayer = Banda "Metalica" ["epica"] 65 trashMetal

--FUNCIONES QUE REALIZAN LAS BANDAS

metal :: EstadoDeAnimo ->  Genero
metal nuevoEstadoDeAnimo = aumentarPublicoMetalero.modificarEstadoAnimo nuevoEstadoDeAnimo 

aumentarPublico :: (Int -> Int) -> Festival -> Festival
aumentarPublico funcion festival = festival {cantidadPublico = funcion.cantidadPublico $ festival}

aumentarPublicoMetalero :: Festival -> Festival
aumentarPublicoMetalero = aumentarPublico (\cantidad -> cantidad + (1 * cantidad `div` 100))

evaluarIndiferencia :: Festival -> Festival
evaluarIndiferencia festival 
    | (=="indiferente"). estadoDeAnimo $festival  = modificarEstadoAnimo "euforico" .aumentarPublico (*2)  $ festival
    | otherwise = festival

modificarEstadoAnimo :: String -> Festival -> Festival
modificarEstadoAnimo palabra festival = festival { estadoDeAnimo = palabra }

--GENEROS
type Metal = Genero

rockNacional ::  Genero
rockNacional = aumentarPublico (+ 100)

pop :: Genero 
pop = evaluarIndiferencia

heavyMetal :: Metal
heavyMetal = aumentarPublicoMetalero.modificarEstadoAnimo "pesado"

trashMetal :: Metal
trashMetal = aumentarPublicoMetalero.modificarEstadoAnimo "basura"

tocar :: Banda -> Festival -> Festival
tocar banda festival= genero banda $ festival

--PUNTO 3
theStrokes :: Banda
theStrokes = Banda "The Strokes" ["suicidio asistido", "emocional", "linda"] 45 fusionRara

fusionRara :: Genero
fusionRara  = heavyMetal.pop

--PUNTO 4

suceder :: Festival -> Festival
suceder festival = foldr ($) festival (map (genero) (bandasQueTocan festival))
suceder' unFestival = foldl (flip tocar) unFestival (bandasQueTocan unFestival)

--PUNTO 5

--criterios de clasificacion de bandas
type Clasificacion = Banda -> Bool

vendida :: Clasificacion
vendida banda = tieneMasDeTresDescripciones banda || tieneDescripcionVendida banda

acustica :: Clasificacion
acustica = tocaAMasDe 55

legendaria :: Clasificacion
legendaria banda= tieneDescripcion "legendaria" banda && tocaAMasDe 40 banda

tieneDescripcionVendida :: Banda -> Bool
tieneDescripcionVendida = tieneDescripcion "vendida"

tieneDescripcion :: String-> Banda -> Bool
tieneDescripcion nombre = elem nombre.descripcion

tieneMasDeTresDescripciones :: Banda -> Bool
tieneMasDeTresDescripciones = (>3).length.descripcion

tocaAMasDe :: Int -> Banda -> Bool
tocaAMasDe cantidad = (>cantidad).decibelesQueToca

--PUNTO 6

popularidad :: Banda -> [Clasificacion] -> Int
popularidad banda  = (*100).length.(clasificacionesQueCumple banda)

clasificacionesQueCumple :: Banda -> [Clasificacion] -> [Clasificacion]
clasificacionesQueCumple banda clasificaciones = filter (\clasificacion -> clasificacion banda) clasificaciones 

--PUNTO 7
{-Definir la función buenFest , que dado un festival y un conjunto de clasificaciones posibles
dice si es un buen fest. Esto sucede cuando cronológicamente cada banda es más popular
que la anterior, y además la popularidad total (la popularidad acumulada de sus bandas)
supera los 1000 puntos.-}

buenFest :: Festival -> [Clasificacion] -> Bool
buenFest (Festival _ _ _ bandasQueTocan) clasificaciones = 
        esMasPopularQueLaOtra bandasQueTocan clasificaciones && popularidadTototal bandasQueTocan clasificaciones > 1000

esMasPopularQueLaOtra :: [Banda] -> [Clasificacion] -> Bool
esMasPopularQueLaOtra [] _ = True
esMasPopularQueLaOtra [unaBanda] _ = True
esMasPopularQueLaOtra (unaBanda:otraBanda:bandas) clasificaciones= esMasPopular unaBanda otraBanda clasificaciones && esMasPopularQueLaOtra bandas clasificaciones

esMasPopular :: Banda -> Banda -> [Clasificacion] -> Bool
esMasPopular unaBanda otraBanda clasificaciones = popularidad unaBanda clasificaciones > popularidad otraBanda clasificaciones

popularidadTototal :: [Banda] -> [Clasificacion] -> Int
popularidadTototal bandas clasificaciones = sum.map (flip popularidad clasificaciones) $ bandas

--PUNTO 8 
{-¿ Fueron de utilidad los conceptos de aplicación parcial y composición? Indicar donde se los
utilizó y justificar su impacto en la solución.-}

{-
Fueron muy utiles en toda la resolución. Se utilizaron para el modelado del genero de las distintas bandas
-}

--PUNTO 9 

{-¿ Sería posible que alguna de las listas fuera infinita y de todas maneras las funciones sigan
funcionando correctamente? Justificar y ejemplificar.
-}
{-
No seria posible que las funciones sigan funcionando correctamente, ya que  en el ejemplo de
las clasificacioneQueCumplen se realiza un filtrado, y si la lista es infinita no terminaria nunca
de filtrar
-}