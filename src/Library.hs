module Library where
import PdePreludat

doble :: Number -> Number
doble x = 2 * x


data Grupo = UnGrupo String Number [Number] deriving Show
getListaAcomp (UnGrupo _ _ listaAcomp) = listaAcomp
getEdadEncargado (UnGrupo _ edadEncargado _) = edadEncargado
getNombreEncargado (UnGrupo nombreEncargado _ _) = nombreEncargado


-- datos de grupos
grupoA :: Grupo 
grupoA = UnGrupo "Pedro" 20 [18, 16]

grupoB :: Grupo 
grupoB = UnGrupo "Leo" 53 [20, 20, 18]

grupoC :: Grupo 
grupoC = UnGrupo "Maria" 30 []


-- FUNCIONES
contarIntegrantes ::Grupo -> Number 
contarIntegrantes grupo = (length (getListaAcomp grupo)) + 1

sumaEdadesGrupo :: Grupo -> Number
sumaEdadesGrupo grupo = (sum (getListaAcomp grupo) + getEdadEncargado grupo)

promedioEdadGrupo :: Grupo -> Number
promedioEdadGrupo grupo = sumaEdadesGrupo grupo /contarIntegrantes grupo

grupoMayorPromEdad :: Grupo -> Grupo -> String
grupoMayorPromEdad grupo1 grupo2 |(promedioEdadGrupo grupo1 > promedioEdadGrupo grupo2) = getNombreEncargado grupo1
                                 | (promedioEdadGrupo grupo2 > promedioEdadGrupo grupo1) = getNombreEncargado grupo2

