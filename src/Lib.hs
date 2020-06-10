module Lib where
import Text.Show.Functions

laVerdad = True

data Festival = Festival {
    lugarDondeSeRealiza :: String,
    cantidadPublico :: Int,
    estadoDeAnimo :: EstadoDeAnimo,
    bandasQueTocan :: [String]
} deriving (Show, Eq)

hullabalooza :: Festival
hullabalooza = Festival "Springfield" 20000 "indiferente" ["Miranda", "Los Redondos", "Metalica", "Soda"]

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

