module PracticaParcial where
import PdePreludat

-- 1 Modelado de personas y criaturas
type Debilidad = (Persona -> Bool) 

data Persona = UnaPersona {nombrePersona :: String,
                           edad :: Number,
                           items :: [String],
                           experiencia :: Number} deriving (Show, Eq)
            
data Criatura = UnaCriatura {nombreCriatura :: String,
                             peligrosidad :: Number,
                             debilidad :: Debilidad
                             } deriving Show

-- defino a las criaturas
tieneDebilidad :: String -> Persona -> Bool 
tieneDebilidad debilidadCriatura persona = elem debilidadCriatura (items persona) -- funcion que me dice si la persona tiene el requisito entre los items para matar a la criatura

siempreFalso _= False --Hago esta funcion para que siempre me de falso no importa lo que le pase entonces en el siempreDetras 

siempreDetras = UnaCriatura {nombreCriatura = "siempredetras", peligrosidad = 0, debilidad = siempreFalso }

gnomos conjuntoGnomos = UnaCriatura {nombreCriatura = "Gnomos", peligrosidad = 2^conjuntoGnomos, debilidad = tieneDebilidad "soplador de hojas"}

-- al fsntasma le paso tambien un reuqisito porque dependiendo de la cateoria tiene una debilidad diferente
fantasmas categoria debilidadCriatura = UnaCriatura {nombreCriatura = "Fantasma", peligrosidad = categoria*20, debilidad = debilidadCriatura}


-- 2 enfrentar persona con criatura

personaContraCriatura :: Persona -> Criatura -> Persona
personaContraCriatura persona criatura | (debilidad criatura) persona  = persona {experiencia = (experiencia persona)+(peligrosidad criatura)}
                                       | otherwise = persona {experiencia = (experiencia persona) +1} -- caso en el que se escapa

persona = UnaPersona {nombrePersona = "Pedro", edad = 19, items = ["hacha", "soplador de hojas"], experiencia = 12}

-- 3 enfrentar persona con muchas criaturas
--sumarExperiencias experieciaPersona = map sum experienciaPersona

--filtrarExperiencias listaExperienciasPersona persona = map (filter (experiencia persona > 0)) listaExperienciasPersona 

--adquirirExperiencia persona grupoCriaturas = filtrarExperiencias (map (personaContraCriatura persona) grupoCriaturas) persona

sumarExperiencias experiencias = sum experiencias

filtrarExperiencias listaExperienciasPersona = sumarExperiencias (map experiencia listaExperienciasPersona)

adquirirExperiencia persona grupoCriaturas = filtrarExperiencias (map (personaContraCriatura persona) grupoCriaturas)