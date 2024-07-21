module Library where
import PdePreludat
import Data.Char (toLower)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{----------------------------------------------Autores y Obras-----------------------------------------------} 
-------------------------------------------------Punto 1 (ok)-------------------------------------------------
{-Modelar las siguientes obras y que existan autores que las hayan publicado:
       1. "Había una vez un pato.", publicada en 1997
       2. "¡Habia una vez un pato!", publicada en 1996
       3. "Mirtha, Susana y Moria.", publicada en 2010
       4. "La semántica funcional del amoblamiento vertebral es riboficiente", publicada en 2020
       5. "La semántica funcional de Mirtha, Susana y Moria.", publicada en 2022-}
{------------------------------------------------------------------------------------------------------------}
--Modelo de Autor
{------------------------------------------------------------------------------------------------------------}
--Modelo de obra
{------------------------------------------------------------------------------------------------------------}
--Declaracion de Obras

{------------------------------------------------------------------------------------------------------------}
--Declaracion de autores

{------------------------------------------------------------------------------------------------------------}
-------------------------------------------------Punto 2 (F)--------------------------------------------------
{-Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y 
quitar signos de puntuación y todo carácter que no sea una letra o un número. 
Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"-}
{------------------------------------------------------------------------------------------------------------}

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
{----1----}
{------------------------------------------------------------------------------------------------------------}
{----2----}
{------------------------------------------------------------------------------------------------------------}
{----3----}
{------------------------------------------------------------------------------------------------------------}
{----4----}

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-----------------------------------------------------Bots---------------------------------------------------}
--------------------------------------------------Punto 4 (ok)------------------------------------------------
{-Modelar dos bots de ejemplo, incluyendo todas las formas de detección existentes hasta ahora.-}
{------------------------------------------------------------------------------------------------------------}

--------------------------------------------------Punto 5 (R+)------------------------------------------------
{-Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.-}
{------------------------------------------------------------------------------------------------------------}

--------------------------------------------------Punto 6 (R+)------------------------------------------------
{-Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo 
plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando 
alguna de sus obras es plagio de alguna de las del otro según el bot.-}
{------------------------------------------------------------------------------------------------------------}

--------------------------------------------------Punto 7 (F)-------------------------------------------------
{-Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  
que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros 
autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.-}
{------------------------------------------------------------------------------------------------------------}
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{--------------------------------------------------Infinito--------------------------------------------------}
--------------------------------------------------Punto 8 (R+)------------------------------------------------
{-Codificar una obra infinita. ¿Qué sucede si se desea verificar si esa obra es plagio de otra con cada 
una de las formas existentes? Justificar conceptualmente en cada caso.-}
{------------------------------------------------------------------------------------------------------------}
