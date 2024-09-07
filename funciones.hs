
-- IMPORTACIONES

import Data.List (nub, intersect)

-- DECLARACION DE FUNCIONES
-------------------------------------------------------------------------------------------------------------------------------------------
-- COMENTARIOS:

-- Primero se declara la firma de tipo de la funcion, especificando el nombre, que tipos de argumentos toma y que tipo de valor retorna

-- Ejemplo: unionConjuntos :: [Int] -> [Int] -> [Int]

-- 'unionConjuntos' es el nombre de la funcion
-- '::' se utiliza para especificar que tipo de argumentos toma y que tipo de valor devuelve la funcion
-- '[Int]' denota una LISTA de elementos de tipo 'Int'
-- El primer  '[Int]' hace referencia a que el primer argumento que toma la funcion debe ser de tipo '[Int]'
-- El segundo '[Int]' hace referencia a que el segundo argumento que toma la funcion debe ser de tipo '[Int]'
-- El ultimo  '[Int]' hace referencia a que la funcion devuelve un valor de tipo 'Int'
-- '->' separa los tipos de los argumentos y el tipo de retorno.

-- Luego se define el cuerpo de la funcion, que describe como se calcula el resultado.

-- Ejemplo: unionConjuntos x y = nub(x ++ y)

-- 'nub' es una función del módulo Data.List que se utiliza para eliminar elementos duplicados de una lista
-- 'nub(x ++ y)' combina las listas x e y y luego elimina los duplicados del resultado.
-------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 1.a).2
unionConjuntos :: [Int] -> [Int] -> [Int]
unionConjuntos x y = nub(x ++ y)
-- Ejercicio 1.b).2
interseccionConjuntos :: [Int] -> [Int] -> [Int]
interseccionConjuntos x y = x `intersect` y
-- Ejercicio 1.d).2
complementoRelativo :: [Int] -> [Int] -> [Int]
complementoRelativo x y = filter (`notElem` y) x
-- Ejercicio 1.c).2
diferenciaSimetrica :: [Int] -> [Int] -> [Int]
diferenciaSimetrica x y = (x `complementoRelativo` y) `unionConjuntos` (y `complementoRelativo` x)
-- Ejercicio 2.a).1
funcionCuadrado :: Int -> Int
funcionCuadrado x = x^2
-- Ejercicio 2.a).2
funcionParSiguiente :: Int -> Maybe Int
funcionParSiguiente x
    | even x = Just (x + 2) -- Si x es par, devuelve el siguiente par
    | otherwise = Nothing   -- Si x es impar, devuelve Nothing (equivalente a un error)
-- Ejercicio 2.a).3
funcionInversoMultiplo3 :: Int -> Maybe Float
funcionInversoMultiplo3 x
    | x `mod` 3 == 0 = Just (1 / fromIntegral x) -- Si x es multiplo de 3, devuelve el inverso de x
    | otherwise = Nothing                        -- Si x NO es multiplo de 3, devuelve Nothing (equivalente a un error)
-- Ejercicio 2.c).1
-- En haskell, 'a' refiere a un tipo de dato generico
-- La restriccion de tipo de dato 'Num a =>' ⋮ 'a' debe ser un tipo de dato de la clase 'Num'
-- La clase de tipos de datos 'Num' incluye tipos de datos que permitan realizar operaciones aritmeticas
f :: Num a => a -> a 
f x = x * x
g :: Num a => a -> a
g x = x + 1
-- Ejercicio 2.c).2
funcionCompuesta :: (a -> a) -> (a -> a) -> a -> a
funcionCompuesta f g x = f (g x) 

-- PROGRAMA PRINCIPAL

main :: IO ()
main = do
-- Ejercicio 1.[a),b),c),d)].1
    let u = [1,2,3,4,5] -- 'u' es una lista de elementos de tipo 'Int' que representa el conjunto universo
    let a = [1,2,3]     -- 'a' es una lista de elementos de tipo 'Int' que representa el conjunto A
    let b = [3,4,5]     -- 'b' es una lista de elementos de tipo 'Int' que representa el conjunto B
-- Ejercicio 1.a).[3-4]
    let unionAB = unionConjuntos a b
    print unionAB
-- Ejercicio 1.b).[3-4]
    let interseccionAB = interseccionConjuntos a b
    print interseccionAB
-- Ejercicio 1.c).[3-4]
    let diferenciaSimetricaAB = diferenciaSimetrica a b
    print diferenciaSimetricaAB
-- Ejercicio 1.d).[3-4]
    let complementoRelativoAU = complementoRelativo u a
    print complementoRelativoAU
-- Ejercicio 2.b).[1,2,3]
    let valorFuncionCuadrado4 = funcionCuadrado 4
    let valorFuncionParSiguiente5 = funcionParSiguiente 5
    let valorFuncionInversoMultiplo36 = funcionInversoMultiplo3 6
-- Ejercicio 2.b).4
    print valorFuncionCuadrado4
    print valorFuncionParSiguiente5
    print valorFuncionInversoMultiplo36
-- Ejercicio 2.c).3
    let funcionCompuestaFG_4 = funcionCompuesta f g 4
    print funcionCompuestaFG_4
    