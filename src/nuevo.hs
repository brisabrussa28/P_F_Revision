module Library where
import PdePreludat
import Data.Char (toLower)
{-
---Autores y Obras
--Modelo de Autor
data Autor = UnAutor{
    nombre_autor :: String,
    obras :: [Obra]
} deriving Show

--Modelo de obra
data Obra = UnaObra{
    nombre_obra :: String,
    fecha_publicacion :: Number
} deriving Show

--Declaracion de Obras
pato1 :: Obra
pato1 = UnaObra{
    nombre_obra = "Había una vez un pato.",
    fecha_publicacion = 1997
}

pato2 :: Obra
pato2 = UnaObra{
    nombre_obra = "¡Habia una vez un pato!",
    fecha_publicacion = 1996
}

msm :: Obra
msm = UnaObra{
    nombre_obra = "Mirtha, Susana y Moria.",
    fecha_publicacion = 2010
}

semantivariboficiente :: Obra
semantivariboficiente = UnaObra{
    nombre_obra = "La semántica funcional del amoblamiento vertebral es riboficiente",
    fecha_publicacion = 2020
}

semantica_msm :: Obra
semantica_msm = UnaObra{
    nombre_obra = "La semántica funcional de Mirtha, Susana y Moria.",
    fecha_publicacion = 2022
}

--Declaracion de autores
robert :: Autor
robert = UnAutor{
    nombre_autor = "Robert Tuti",
    obras = [pato1,msm]
}

miriam :: Autor
miriam = UnAutor{
    nombre_autor = "Miriam Lopez",
    obras = [pato2,semantica_msm]
}

tito :: Autor
tito = UnAutor{
    nombre_autor = "Tito X",
    obras = [semantivariboficiente]
}

---Plagios
largoObra :: Obra -> Number
largoObra = length.nombre_obra

tomarPrimerosNCaracteresObra :: Number -> Obra -> String
tomarPrimerosNCaracteresObra numero obra = (take numero) (nombre_obra obra)

diferenciaEntreObras :: Obra -> Obra -> Number
diferenciaEntreObras obra_1 obra_2 = largoObra obra_1 - largoObra obra_2

tomarUltimosNCaracteres :: Number -> Obra -> String
tomarUltimosNCaracteres numero obra = drop (numero) (nombre_obra obra)

compararString :: String -> String -> Bool
compararString str1 str2 = str1 == str2

type Plagio = Obra -> Obra -> Bool

copiaLiteral :: Plagio
copiaLiteral obra_1 obra_2 = compararString (nombre_obra obra_1 ) (nombre_obra obra_2)

empiezanIgual :: Number -> Plagio
empiezanIgual numero obra_1 obra_2 = compararString (tomarPrimerosNCaracteresObra numero obra_1) (tomarPrimerosNCaracteresObra numero obra_2)

agregaronIntro :: Plagio
agregaronIntro obra_1 obra_2 = compararString (tomarUltimosNCaracteres (diferenciaEntreObras obra_1 obra_2) obra_1) (nombre_obra obra_2)

---Bots
--Modelado de Bots
data Bot = UnBot {
    nombre_fabricante :: String,
    plagios :: [Plagio]
} deriving Show

--declaracion de Bots
bot1 :: Bot
bot1 = UnBot{
    nombre_fabricante = "Acme",
    plagios = [copiaLiteral, agregaronIntro]
}

bot2 :: Bot
bot2 = UnBot{
    nombre_fabricante = "HP",
    plagios = [copiaLiteral]
}

--Si el bot detecta plagio
detectoPlagio :: [Plagio] -> Obra -> Obra -> Bool
detectoPlagio [x] obra_1 obra_2 = x obra_1 obra_2
detectoPlagio [] obra_1 obra_2 = False
detectoPlagio (x:y:xy) obra_1 obra_2 
    |(x obra_1 obra_2) == False = detectoPlagio (y:xy) obra_1 obra_2
    |(x obra_1 obra_2) = True

botDetectoPlagio :: Bot -> Obra -> Obra -> Bool
botDetectoPlagio bot = detectoPlagio (plagios bot)

--Cadena de Plagiadores ordenados
obrasRepetidas :: Bot -> [Obra] -> Obra -> Bool
obrasRepetidas bot obras obra = any (detectoPlagio (plagios bot) obra) obras

alMenosUnaListaRepetida :: [Obra] -> [Obra] -> Bot -> Bool
alMenosUnaListaRepetida [] obras bot = False
alMenosUnaListaRepetida [x] obras bot = obrasRepetidas bot obras x
alMenosUnaListaRepetida (x:y:xy) obras bot
    |obrasRepetidas bot obras x == False = alMenosUnaListaRepetida (y:xy) obras bot
    |obrasRepetidas bot obras x = True

todosPlagiadores :: [Autor] -> Bot -> Bool
todosPlagiadores [] bot = False
todosPlagiadores [x] bot = False
todosPlagiadores (x:y:xy) bot = alMenosUnaListaRepetida (obras x) (obras y) bot && todosPlagiadores (y:xy) bot

--Lista de autores redimidos


---Infinito
obraInfinita :: Obra
obraInfinita = UnaObra{
    fecha_publicacion = 0,
    nombre_obra = cycle "Infinito"
}

{-
--Copia literal
Si comparo la obra infinita con una obra finita va devolver un booleano, porque recorrerá cada caracter hasta llegar al final de la oracion finita y 
al no existir más elementos devolverá False. En cambio, si compara dos cadenas infinitas puede variar, si estás son distintas en algun elemento 
devolverá False. En el caso de ser ambas iguales, jamas dejará de comparar elementos y no podrá dar un resultado.
--empiezanIgual
Siempre devolverá un valor ya que toma los primeros elementos de cada cadena sin importar su largo.
--agregaron Intro
No podrá devolver un valor jamás ya que no puede comparar el largo de ambas cadenas.
-}-}