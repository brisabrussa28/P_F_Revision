module Library where
import PdePreludat
import Data.Char (toLower)
--para abrir library.hs: stack ghci
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-En un sistema de registro de obras literarias, existen autores que producen obras y bots que detectan eventuales plagios.
De los autores, además de sus obras, se conoce también su nombre, y las obras se representan con un texto y un año de publicación.-}


{----------------------------------------------Autores y Obras-----------------------------------------------} 
-------------------------------------------------Punto 1 (ok)-------------------------------------------------
{-Modelar las siguientes obras y que existan autores que las hayan publicado:
       1. "Había una vez un pato.", publicada en 1997
       2. "¡Habia una vez un pato!", publicada en 1996
       3. "Mirtha, Susana y Moria.", publicada en 2010
       4. "La semántica funcional del amoblamiento vertebral es riboficiente", publicada en 2020
       5. "La semántica funcional de Mirtha, Susana y Moria.", publicada en 2022-}
{------------------------------------------------------------------------------------------------------------}
--Modelo de obra
data Obra = UnaObra{
    titulo :: String,
    año_publicacion :: Number
} deriving Show

{------------------------------------------------------------------------------------------------------------}
--Declaracion de Obras
o_pato_1997 :: Obra
o_pato_1997 = UnaObra "Había una vez un pato." 1997

o_pato_1996 :: Obra
o_pato_1996 = UnaObra "¡Habia una vez un pato!" 1996

mSM :: Obra
mSM = UnaObra "Mirtha, Susana y Moria." 2010

semanticaV :: Obra
semanticaV = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

semanticaMSM :: Obra
semanticaMSM  = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

{------------------------------------------------------------------------------------------------------------}
--Modelo de Autor
data Autor = UnAutor{
    nombre :: String,
    obras :: [Obra]
}deriving Show

{------------------------------------------------------------------------------------------------------------}
--Declaracion de autores
cesar :: Autor
cesar = UnAutor "Julio Cesar" [o_pato_1997]

cortazar :: Autor
cortazar = UnAutor "Julio Cortazar" [o_pato_1996, semanticaMSM]

trump :: Autor
trump = UnAutor "Donald Trump" [semanticaV, mSM]
{------------------------------------------------------------------------------------------------------------}
-------------------------------------------------Punto 2 (F)--------------------------------------------------
{-Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y 
quitar signos de puntuación y todo carácter que no sea una letra o un número. 
Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"-}
{------------------------------------------------------------------------------------------------------------}
letrasYNumeros :: [Char]
letrasYNumeros = ['a'..'z']++['A'..'Z']++['0'..'9']++[' ']

quitarCaracteresEspeciales :: String -> String
quitarCaracteresEspeciales = filter (`elem` letrasYNumeros)

quitarAcento :: Char -> Char
quitarAcento 'á' = 'a'
quitarAcento 'é' = 'e'
quitarAcento 'í' = 'i'
quitarAcento 'ó' = 'o'
quitarAcento 'ú' = 'u'
quitarAcento 'Á' = 'u'
quitarAcento 'É' = 'u'
quitarAcento 'Í' = 'u'
quitarAcento 'Ó' = 'u'
quitarAcento 'Ú' = 'u'
quitarAcento letra = letra

--purificarTexto (titulo o_pato_1997)
purificarTexto :: String -> String
purificarTexto = quitarCaracteresEspeciales.map quitarAcento
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{--------------------------------------------------Plagios---------------------------------------------------}
-------------------------------------------------Punto 3 (R+)-------------------------------------------------
{-Se desea detectar si una obra es plagio de la otra. Hay distintas formas de reconocer un plagio, 
de los cuales se conocen las siguientes, pero podrían haber más. En cualquier caso, una obra debe haber 
sido publicada en un año posterior a la obra original para ser considerada un plagio. 
    1. Copia literal: ocurre cuando la versión cruda de una es igual a la de la otra. 
        Por ejemplo, A es plagio de B.
    2. Empieza igual: Los primeros caracteres de una obra son los mismos que otra, y su longitud es menor. 
    La cantidad de caracteres a analizar puede ser variable. 
        Por ejemplo, E es plagio de D para una cantidad 10, pero no para una cantidad 30.
    3. Le agregaron intro: La obra plagiada empieza a su manera, pero al final incluye totalmente el 
    texto de la original. 
        Por ejemplo, E es plagio de C.
    4. Inventar otra forma de detectar plagio, utilizando una expresión lambda.-}
{------------------------------------------------------------------------------------------------------------}
--Type Plagios
type Plagio = Obra -> Obra -> Bool

--Funciones auxiliares
cantidadAgregado :: Obra -> Obra -> Number
cantidadAgregado obra_1 obra_2 = length (purificarTexto (titulo obra_1))-length (purificarTexto (titulo obra_2))

{----1----}
--obra_1 es copia literal de obra_2
copiaLiteral :: Plagio
copiaLiteral obra_1 obra_2 = purificarTexto (titulo obra_1) == purificarTexto (titulo obra_2)

{------------------------------------------------------------------------------------------------------------}
{----2----}
--obra_1 empieza igual a obra_2
empiezaIgual :: Number -> Plagio
empiezaIgual numero obra_1 obra_2 = take numero (purificarTexto (titulo obra_1)) == take numero (purificarTexto (titulo obra_2))

{------------------------------------------------------------------------------------------------------------}
{----3----}
--obra_1 termina igual a obra_2
leAgregaronIntro :: Plagio
leAgregaronIntro obra_1 obra_2 = drop (cantidadAgregado obra_1 obra_2) (purificarTexto (titulo obra_1)) == purificarTexto (titulo obra_2)

{------------------------------------------------------------------------------------------------------------}
{----4----}
--Si todas las letras del titulo de la obra1 existen en el titulo de la obra_2
letrasIguales :: Plagio
letrasIguales obra_1 obra_2 = all(\letra -> letra `elem` purificarTexto (titulo obra_2)) (purificarTexto (titulo obra_1))


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-----------------------------------------------------Bots---------------------------------------------------}
--------------------------------------------------Punto 4 (ok)------------------------------------------------
{-Modelar dos bots de ejemplo, incluyendo todas las formas de detección existentes hasta ahora.-}
{------------------------------------------------------------------------------------------------------------}
--Modelado de bots
newtype Bot
  = UnBot {formasDeteccion :: [Plagio]}
  deriving Show

--Declaracion de bots
bot_1 :: Bot
bot_1 = UnBot [copiaLiteral, empiezaIgual 10]

bot_2 :: Bot
bot_2 = UnBot [leAgregaronIntro]

--------------------------------------------------Punto 5 (R+)------------------------------------------------
{-Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.-}
{------------------------------------------------------------------------------------------------------------}
--Obra1 es plagio de Obra2 si el plagio dio positivo y Obra1 salió despues de Obra2
aplicarPlagio :: Obra -> Obra -> Plagio ->  Bool
aplicarPlagio obra_1 obra_2 plagio = plagio obra_1 obra_2 && (año_publicacion obra_1 > año_publicacion obra_2)

--El bot detecta si Obra1 es plagio de Obra2
botDetectarPlagio :: Bot -> Obra -> Obra -> Bool
botDetectarPlagio bot obra_1 obra_2 = any (aplicarPlagio obra_1 obra_2) (formasDeteccion bot)

--------------------------------------------------Punto 6 (R+)------------------------------------------------
{-Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo 
plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando 
alguna de sus obras es plagio de alguna de las del otro según el bot.-}
{------------------------------------------------------------------------------------------------------------}
--El bot detecta si la obra ingresada es plagio del autor ingresado
autorPlagio :: Autor -> Bot -> Obra -> Bool
autorPlagio autor bot obra = any (botDetectarPlagio bot obra) (obras autor)

--El bot detecta si el autorS hizo plagio al autorO
autorSPlagioAutorO :: Autor -> Bot -> Autor -> Bool
autorSPlagioAutorO autor_s bot autor_o = any (autorPlagio autor_o bot) (obras autor_s)

--El bot detecta si es una cadena de plagiadores
esCadenaDePlagiadores :: [Autor] -> Bot -> Bool
esCadenaDePlagiadores [] bot = False
esCadenaDePlagiadores [x,y] bot = autorSPlagioAutorO x bot y 
esCadenaDePlagiadores (x:y:xy) bot = autorSPlagioAutorO x bot y  && esCadenaDePlagiadores xy bot

--------------------------------------------------Punto 7 (F)-------------------------------------------------
{-Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  
que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros 
autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.-}
{------------------------------------------------------------------------------------------------------------}
--Si la obra es plagio de algun autor de la lista
obraPlagioDeAlgunAutor :: [Autor] -> Bot -> Obra -> Bool
obraPlagioDeAlgunAutor autores bot obra = any(\autor -> autorPlagio autor bot obra) autores

--Lista de obras plagio segun la lista de autores a comparar
obrasPlagioDelAutor :: Autor -> [Autor] -> Bot -> [Obra]
obrasPlagioDelAutor autor autores bot = filter(obraPlagioDeAlgunAutor autores bot) (obras autor)

--Si solo plagió una vez y nunca más es que ya aprendió
autorYaNoPlagia :: Autor -> [Autor] -> Bot -> Bool
autorYaNoPlagia autor autores bot = length(obrasPlagioDelAutor autor autores bot) == 1

--Lista de autores que plagiaron pero aprendieron
autoresQueYaNoPlagian :: [Autor] -> Bot -> [Autor]
autoresQueYaNoPlagian autores bot = filter (\autor -> autorYaNoPlagia autor autores bot) autores

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{--------------------------------------------------Infinito--------------------------------------------------}
--------------------------------------------------Punto 8 (R+)------------------------------------------------
{-Codificar una obra infinita. ¿Qué sucede si se desea verificar si esa obra es plagio de otra con cada 
una de las formas existentes? Justificar conceptualmente en cada caso.-}
{------------------------------------------------------------------------------------------------------------}
--------------Obra infinita
obraInfinita :: Obra
obraInfinita = UnaObra (cycle "Obra infinita... ") 2000

obraInfinita2 :: Obra
obraInfinita2 = UnaObra (cycle "X ") 2000
{------------------------------------------------------------------------------------------------------------}
--------------Respuestas
--copiaLiteral
{-En este caso existen tres posibles caso. 
Si se compara la obra infinita con una finita, gracias al Lazy Evaluation, una vez se terminan los elementos comparables de una
de las dos cadenas, queda claro que no son iguales y da False.
Si se comparan dos obras infinitas distintas, El Lazy Evaluation al encontrar tan solo un elemento distinto, cortara el proceso y dará False sin importar si existen aun elementos
sin comparar.
Si se comparan dos listas infinitas iguales, jamás dará resultado y no habrá Lazy Evaluation, ya que no podrá jamás dejar de comparar elementos.-}

--empiezaIgual
{-Gracias al Lazy Evaluation solo toma una cantidad finita de letras sin tomar en cuenta el largo de las cadenas, 
por lo que dará un resultado sin problema el cual puede ser verdadero o falso.-}

--leAgregaronIntro
{-En este caso no existe Lazy Evaluation ya que no puede calcular el largo de la obra infinita, por lo que no existe resultado posible.-}

--letrasIguales
{-Aquí pueden ocurrir cuatro cosas.
Si realizo letrasIguales obraInfinita o_pato_1996, dará falso, porque gracias al Lazy Evaluation, al encontrar una sola letra de la obra infinita que no esté en
la segunda obra, dará el resultado ya que no será posible que todas las letras se encuentren.
Si comparo cada letra de la obraInfinita con una obra que tenga el mismo texto que se reíte en ciclo, jamás podrá dar falso, ya que todas las letras existen dentro de la cadena finita
y no podrá dejar de comparar. No hay lazy evaluation.
Si comparo dos obras infinitas, no importa si son distintas, porque buscara infinitamente de todas formas, impidiendo dar un resultado. No hay lazy evaluation.
Si realizo letrasIguales o_pato_1996 obraInfinita, jamás dará resultado, porque buscará de forma infinita las letras del titulo finito dentro del infinito. No hay lazy evaluation.-}