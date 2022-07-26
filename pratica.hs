-- Fatorial de duas formas (primeira forma)
la::Int->Int
la n = if n == 0 then 1
else la (n-1) * n
-- Segunda forma
fat::Int->Int
fat n
    | n == 0 = 1
    | otherwise = fat (n-1)*n
-- Calculando a soma de uma lista
calcusoma::[Int]->Int
calcusoma (x:xs)
    | xs == [] = x
    | otherwise = x + calcusoma xs
-- Tamanho de uma lista
cal::[Int]->Int
cal [] = 0
cal (_:xs) = 1 + cal xs
-- números impares de uma lista
vaa::[Int]->[Int]
vaa mm = [ a | a <- mm, odd (a)]
-- números pares de uma lista
va::[Int]->[Int]
va mm = [ a | a <- mm, odd a == False]
-- Números impares sem ZF
emZf::[Int]->[Int]
emZf [] = []
emZf (x:xs)
    | odd (x) = x:emZf xs 
    | otherwise = emZf xs
-- ordenação de lista
s::[Int]->[Int]
s [] = []
s (x:xs) = s [y | y<-xs, y<x ] ++ [x] ++ s [y | y<-xs, y>=x]
--Comparando duas listas
compr::[Int]->[Int]->Bool
compr [] [] = True
compr (x:xs) (y:ys)
    | x /= y = False
    | otherwise = compr xs ys
--Concatenando duas listas
comb::[Int]->[Int]->[Int]
comb ls sl = ls ++ sl
--Primeira Questão
--Definição Polimorfa
minhaFuncao :: Num a => a -> a
minhaFuncao x = 3 + f x + f a + f b
    where
        f x = x + 7*c
        a = 3*c
        b = f 2
        c = 10
--impar ou par 
par :: Int -> Bool
par n 
    | n == 0 = True
    | n > 0 = impar (n-1)
    | otherwise = par (-n)

impar :: Int -> Bool
impar n 
    | n == 0 = False
    | n > 0 = par (n-1)
    | otherwise = impar (-n)

primo::Int -> Bool
primo n
    | n == 1 = False
    |otherwise = auxPrimo n (n-1) 

auxPrimo::Int -> Int -> Bool
auxPrimo n c
    | c == 1 = True
    | mod n c == 0 = False
    | otherwise = auxPrimo n (c-1)

{--1. Você está desenvolvendo um novo algoritmo de compressão de dados em Haskell e deseja
analisar quais dados mais se repetem em uma determinada lista. Sendo assim, você resolve
criar a função analise que organiza os dados analisados em uma lista de tuplas contendo cada
elemento e sua respectiva contagem de repetições. (2,0)
Exemplo:
analise "IFMA CAXIAS" -> [('I',2),('F',1),('M',1),('A',3),(' ',1),('C',1),('A',3),('X',1),('I',2),('A',3), (’S’,1)]--}
as :: [Char]
as = "IFMA CAXIAS"

br :: [Char]
br = "Flamengo campeao"

analise::String->[(Char,Int)]
analise tamm = [(a,sum [1 | b <-tamm, b==a] ) | a <- tamm]
        
{-1. Um número afortunado (homenagem a Reo Fortune) é definido como o menor inteiro
tal que, para um dado inteiro positivo , devemos multiplicar os primeiros primos e somar a
para obter também um número primo. Por exemplo, para encontrar o terceiro número
afortunado, primeiro calcularíamos o produto dos três primeiros primos (2, 3, 5), que é 30. Em
seguida buscaríamos , adicionando 2 ao resultado e obtendo um número par (32),
posteriormente adicionando 3 e obtendo um número múltiplo de 3 (33), adicionando 4 e
obtendo um outro número par (34) e assim sucessivamente até o número 7, quando geramos
um novo número primo (37). Implemente a função que recebe , e
calcula o respectivo número afortunado. (4,0)
Obs: Os primeiros números afortunados são: 3, 5, 7, 13, 23, 17, 19, 23, 37, 61, 67, 61, 71, 47, ...
Exemplos:
a. afortunado 1 -> 3
b. afortunado 5 -> 23-}
afortunado::Int->Int
afortunado n = impri (multP n 2 1) 2

multP::Int->Int->Int->Int--multiplica primos
multP pos n resultado
    | pos == 0 = resultado
    | primo n == True = multP (pos-1) (n+1) (n*resultado)
    | otherwise = multP pos (n+1) resultado

impri::Int->Int->Int
impri produt con
    | primo(produt + con) == True = con
    | otherwise = impri produt (con+1)

{-O Triângulo de Pascal foi estudado pelo matemático chinês Yang Hui (1238-1298), entretanto,
os estudos mais famosos foram do matemático italiano Niccolò Fontana Tartaglia
(1499-1559) e do matemático francês Blaise Pascal (1623-1662), que provou várias de suas
propriedades. Sabendo que o Triângulo de Pascal é produzido a partir do resultado de
combinações matemáticas específicas em cada linha e que elas podem ser representadas por
tuplas, implemente a função em Haskell que receba o número de
uma linha do triângulo e retorne a lista de tuplas correspondente. (4,0)

Exemplos:
a. pascal 0 -> [(0,0)]
b. pascal 4 -> [(4,0),(4,1),(4,2),(4,3),(4,4)]-}
pascal::Int->[(Int,Int)]
pascal linha = [(l,n) | l<-[linha], n<-[0,1..linha]]