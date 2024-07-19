module Library where
import PdePreludat
import Data.Char (toLower)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{----------------------------------------------Autores y Obras-----------------------------------------------} 
-------------------------------------------------Punto 1 (ok)-------------------------------------------------
{------------------------------------------------------------------------------------------------------------}
--Modelo de Autor
data Autor = UnAutor{
    nombre_autor :: String,
    obras :: [Obra]
} deriving Show
{------------------------------------------------------------------------------------------------------------}
--Modelo de obra
data Obra = UnaObra{
    nombre_obra :: String,
    fecha_publicacion :: Number
} deriving Show
{------------------------------------------------------------------------------------------------------------}
--Declaracion de Obras
pato1 :: Obra
pato1 = UnaObra "Había una vez un pato." 1997

pato2 :: Obra
pato2 = UnaObra "¡Habia una vez un pato!" 1996

msm :: Obra
msm = UnaObra "Mirtha, Susana y Moria." 2010

semantivariboficiente :: Obra
semantivariboficiente = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

semanticaMsm :: Obra
semanticaMsm = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022
{------------------------------------------------------------------------------------------------------------}
--Declaracion de autores
robert :: Autor
robert = UnAutor "Robert Tuti" [pato1,msm]

miriam :: Autor
miriam = UnAutor "Miriam Lopez" [pato2,semanticaMsm]

tito :: Autor
tito = UnAutor "Tito X" [semantivariboficiente]
{------------------------------------------------------------------------------------------------------------}
-------------------------------------------------Punto 2 (F)--------------------------------------------------
{-
Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y quitar signos de puntuación y todo carácter que no sea una letra o un número. 
Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"
-}
--Quitar acento de las letras
quitarAcento :: Char -> Char
quitarAcento 'á' = 'a'
quitarAcento 'é' = 'e'
quitarAcento 'í' = 'i'
quitarAcento 'ó' = 'o'
quitarAcento 'ú' = 'u'
quitarAcento 'Á' = 'A'
quitarAcento 'É' = 'E'
quitarAcento 'Í' = 'I'
quitarAcento 'Ó' = 'O'
quitarAcento 'Ú' = 'U'



---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{--------------------------------------------------Plagios---------------------------------------------------}
-------------------------------------------------Punto 3 (R+)-------------------------------------------------
largoObra :: Obra -> Number
largoObra = length.nombre_obra

tomarPrimerosNCaracteresObra :: Number -> Obra -> String
tomarPrimerosNCaracteresObra numero obra = take numero (nombre_obra obra)

diferenciaEntreObras :: Obra -> Obra -> Number
diferenciaEntreObras obra_1 obra_2 = largoObra obra_1 - largoObra obra_2

tomarUltimosNCaracteres :: Number -> Obra -> String
tomarUltimosNCaracteres numero obra = drop numero (nombre_obra obra)

compararString :: String -> String -> Bool
compararString str1 str2 = str1 == str2

type Plagio = Obra -> Obra -> Bool
{------------------------------------------------------------------------------------------------------------}
{----1----}
copiaLiteral :: Plagio
copiaLiteral obra_1 obra_2 = compararString (nombre_obra obra_1 ) (nombre_obra obra_2)
{------------------------------------------------------------------------------------------------------------}
{----2----}
empiezanIgual :: Number -> Plagio
empiezanIgual numero obra_1 obra_2 = compararString (tomarPrimerosNCaracteresObra numero obra_1) (tomarPrimerosNCaracteresObra numero obra_2)
{------------------------------------------------------------------------------------------------------------}
{----3----}
agregaronIntro :: Plagio
agregaronIntro obra_1 obra_2 = compararString (tomarUltimosNCaracteres (diferenciaEntreObras obra_1 obra_2) obra_1) (nombre_obra obra_2)
{------------------------------------------------------------------------------------------------------------}
{----4----}

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-----------------------------------------------------Bots---------------------------------------------------}
--------------------------------------------------Punto 4 (ok)------------------------------------------------
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

--------------------------------------------------Punto 5 (R+)------------------------------------------------
--Si el bot detecta plagio
detectoPlagio :: [Plagio] -> Obra -> Obra -> Bool
detectoPlagio [x] obra_1 obra_2 = x obra_1 obra_2
detectoPlagio [] obra_1 obra_2 = False
detectoPlagio (x:y:xy) obra_1 obra_2 
    |not (x obra_1 obra_2) = detectoPlagio (y:xy) obra_1 obra_2
    |x obra_1 obra_2 = True

botDetectoPlagio :: Bot -> Obra -> Obra -> Bool
botDetectoPlagio bot = detectoPlagio (plagios bot)

--------------------------------------------------Punto 6 (R+)------------------------------------------------
--Cadena de Plagiadores ordenados
obrasRepetidas :: Bot -> [Obra] -> Obra -> Bool
obrasRepetidas bot obras obra = any (detectoPlagio (plagios bot) obra) obras

alMenosUnaListaRepetida :: [Obra] -> [Obra] -> Bot -> Bool
alMenosUnaListaRepetida [] obras bot = False
alMenosUnaListaRepetida [x] obras bot = obrasRepetidas bot obras x
alMenosUnaListaRepetida (x:y:xy) obras bot
    |not (obrasRepetidas bot obras x) = alMenosUnaListaRepetida (y:xy) obras bot
    |obrasRepetidas bot obras x = True

todosPlagiadores :: [Autor] -> Bot -> Bool
todosPlagiadores [] bot = False
todosPlagiadores [x] bot = False
todosPlagiadores (x:y:xy) bot = alMenosUnaListaRepetida (obras x) (obras y) bot && todosPlagiadores (y:xy) bot

--------------------------------------------------Punto 7 (F)-------------------------------------------------
{-
Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  
que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, 
nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.
-}
--Lista de autores redimidos

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{--------------------------------------------------Infinito--------------------------------------------------}
--------------------------------------------------Punto 8 (ok)------------------------------------------------
obraInfinita :: Obra
obraInfinita = UnaObra{
    fecha_publicacion = 0,
    nombre_obra = cycle "Infinito"
}

--------------------------------------------------Punto 9 (R+)------------------------------------------------
{-
--Copia literal
Si comparo la obra infinita con una obra finita va devolver un booleano, porque recorrerá cada caracter hasta llegar al final de la oracion finita y 
al no existir más elementos devolverá False. En cambio, si compara dos cadenas infinitas puede variar, si estás son distintas en algun elemento 
devolverá False. En el caso de ser ambas iguales, jamas dejará de comparar elementos y no podrá dar un resultado.
--empiezanIgual
Siempre devolverá un valor ya que toma los primeros elementos de cada cadena sin importar su largo.
--agregaron Intro
No podrá devolver un valor jamás ya que no puede comparar el largo de ambas cadenas.
-}
