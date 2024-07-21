module Res_profe where
import PdePreludat
import Data.Char (toLower)

data Obra1 = UnaObra1 {
    contenido :: String,
    fecha :: Number
} deriving (Show, Eq)

data Autor1 = UnAutor1{
   nombre :: String,
   obras :: [Obra1]
} deriving (Show,Eq)

--Punto 1
-- A
obraA1 :: Obra1
obraA1 = UnaObra1 "Había una vez un pato." 1997
--B
obraB1 :: Obra1
obraB1 = UnaObra1 "¡Habia una vez un pato!" 1998
--C
obraC1 :: Obra1
obraC1 = UnaObra1 "Mirtha, Susana y Moria." 2010
--D
obraD1 :: Obra1
obraD1 = UnaObra1 "La semántica funcional del amoblamiento vertebral es riboficiente" 2020
--E
obraE1 :: Obra1
obraE1 = UnaObra1 "La semántica funcional de Mirtha, Susana y Moria." 2022

autor11 :: Autor1
autor11 = UnAutor1 "nn1" [obraA1]
autor21 :: Autor1
autor21 = UnAutor1 "nn2" [obraB1, obraC1]
autor31 :: Autor1
autor31 = UnAutor1 "nn3" [obraB1, obraD1]
autor41 :: Autor1
autor41 = UnAutor1 "nn4" [obraE1, obraB1]

--Punto 2
versionCruda1 :: String -> String
versionCruda1 = filter esLetraONumero1.map sinAcento1

vC :: String -> String
vC = filter (`elem` (['a'..'z']++['A'..'Z'] ++ "0123456789 ")).map sinAcento1

esLetraONumero1 :: Char ->  Bool
esLetraONumero1 caracter = caracter `elem` todasLasLetrasYNumeros1

sinAcento1 :: Char ->  Char
sinAcento1 'á' = 'a'
sinAcento1 'é' = 'e'
sinAcento1 'í' = 'i'
sinAcento1 'ó' = 'o'
sinAcento1 'ú' = 'u'
sinAcento1 'Á' = 'A'
sinAcento1 'É' = 'E'
sinAcento1 'Í' = 'I'
sinAcento1 'Ó' = 'O'
sinAcento1 'Ú' = 'U'
sinAcento1 letra = letra


todasLasLetrasYNumeros1 :: [Char]
todasLasLetrasYNumeros1 = ['a'..'z']++['A'..'Z'] ++ "0123456789 "

--plagios
--Punto 3

type FormaDeteccion1 = String ->  String ->  Bool

--a
copiaLiteral1 :: FormaDeteccion1
copiaLiteral1 texto textoOriginal = versionCruda1 texto == versionCruda1  textoOriginal
--b

empiezaIgual1 :: Number ->  FormaDeteccion1
empiezaIgual1 cantidadDeCaracteres texto textoOriginal = take cantidadDeCaracteres texto == take cantidadDeCaracteres textoOriginal &&  length texto < length textoOriginal
--c
leAgregaronIntro1 :: FormaDeteccion1
leAgregaronIntro1 texto textoOriginal = ultimosElementos1 (length textoOriginal)  texto == textoOriginal

ultimosElementos1 :: Number -> String -> String
ultimosElementos1 cant texto  = drop (length texto - cant) texto
--d

-- 
--punto 4
data Bot1 = UnBot1 {
    formas :: [FormaDeteccion1],
    fabricante :: String
} 

botA1 :: Bot1
botA1 = UnBot1 [copiaLiteral1, leAgregaronIntro1, empiezaIgual1 10] "botter"

botB1 :: Bot1
botB1 = UnBot1 [empiezaIgual1 10, leAgregaronIntro1] "botter"

--5. Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

deteccion1 :: Obra1 -> Obra1 -> FormaDeteccion1 -> Bool
deteccion1 obra obraOriginal forma = fecha obra > fecha obraOriginal && forma (contenido obra) (contenido obraOriginal)  

esPlagioDeEstaObra1 :: Bot1 -> Obra1 -> Obra1 -> Bool
esPlagioDeEstaObra1 bot obra obraOriginal = any (deteccion1 obra obraOriginal)  (formas bot)

--6. Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del otro según el bot.

esPlagioDeEsteAutor1 :: Bot1 -> Autor1 ->  Obra1 -> Bool
esPlagioDeEsteAutor1 bot autorOriginal obra = any (esPlagioDeEstaObra1 bot obra) (obras autorOriginal)

plagioA1 :: Bot1 ->  Autor1 ->  Autor1 -> Bool
plagioA1 bot autor autorOriginal = any (esPlagioDeEsteAutor1 bot autorOriginal) (obras autor)

cadenaPlagiadores1 :: Bot1 ->  [Autor1] -> Bool
cadenaPlagiadores1 bot [ _] = False
cadenaPlagiadores1 bot [x1,x2] = plagioA1 bot x1 x2
cadenaPlagiadores1 bot (x1:x2:xs) = plagioA1 bot x1 x2 && cadenaPlagiadores1 bot (x2:xs)

--plagioA bot autor autorOriginal = any (\obra -> any (esPlagioDeEstaObra bot obra) (obras autorOriginal)) (obras autor)

-- 7. Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.

esPlagioDeAlgunoDeEstosAutores1 :: Bot1 -> [Autor1] -> Obra1 -> Bool
esPlagioDeAlgunoDeEstosAutores1  bot autores obra = any (\autor -> esPlagioDeEsteAutor1 bot autor obra) autores

obrasPlagiadasDelAutor1 :: Bot1 -> Autor1 -> [Autor1] -> [Obra1]
obrasPlagiadasDelAutor1 bot autor autores =  filter (esPlagioDeAlgunoDeEstosAutores1 bot autores) (obras autor) 

aprendio1 :: Bot1 -> Autor1 -> [Autor1] -> Bool
aprendio1 bot autor autores =  length (obrasPlagiadasDelAutor1 bot autor autores) == 1

aprendieron1 :: Bot1 -> [Autor1] -> [Autor1]
aprendieron1 bot autores = filter (\a -> aprendio1 bot a (quitar a autores)) autores 
  where quitar x = filter (/= x)
--8---------------------------------------------------
obraInfinita1 :: Obra1
obraInfinita1 = UnaObra1 (repeat 'a') 2021

--9---------------------------------------------------
obraInfinita21 :: Obra1
obraInfinita21 = UnaObra1 (repeat 'a') 2025

{-

Suponiendo una consulta como: deteccion obraA obraInfinita copiaLiteral
No importa cuál sea la forma de detección, como la fecha de la obra que se pregunta si es plagio es anterior, no se cumple y corta diciendo False, por Lazy evaluation no es necesario seguir evaluando el otro lado del &&.

Ahora veamos los casos donde se cumple que la fecha es posterior:

copiaLiteral:
- Suponiendo la consulta: deteccion obraInfinita obraA copiaLiteral
da False, por Lazy Evaluation. Al evaluar la igualdad de contenidos no necesita evaluar toda la lista infinita para saber que los strings son distintos, con los primeros caracteres alcanza.
- Pero si consulto deteccion obraInfinita2 obraInfinita copiaLiteral, se cuelga, porque para saber si dos strings iguales son iguales necesita recorrerlos todos, aún con lazy evaluation.

empiezaIgual:
- Suponiendo la consulta: deteccion obraInfinita obraA empiezaIgual
Entonces da False, pues verifica con take que sean iguales los contenidos y eso da false. Por Lazy evaluation no es necesario evaluar la lista infinita para el take.
- Suponiendo la consulta: deteccion obraInfinita2 obraInfinita empiezaIgual
Ahí se cuelga, porque nunca llega a comparar las longitudes de los contenidos, aún con lazy evaluation. Es decir, aún si una es infinita y la otra empieza igual, jamás cortará.

leAgregaronIntro:
- Suponiendo la consulta: deteccion obraInfinita obraA leAgregaronIntro
Aún teniendo lazy evaluation, para el calcular el length del contenido de la obra infinita se cuelga, antes de poder hacer el drop.
- Ahora, si hacemos al revés: deteccion obraA obraInfinita leAgregaronIntro
Se colgaría, pues se pide hacer un ultimosElementos, que a su vez necesita el length de la lista infinita, no hay Lazy evaluation que lo salve.
-}