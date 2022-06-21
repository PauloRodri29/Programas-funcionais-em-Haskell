auxcont::Int->Int->Int
auxcont n p
   | n == 0 = p-1
   | primo(p) = auxcont(n-1) (p+1)
   | otherwise = auxcont n (p+1)

nnPrimo::Int->Int
nnPrimo n 
  | n == 1 = 2
  |otherwise =  auxcont n 2

--1. Defina a função potencia :: Int -> Int -> Int em Haskell, que calcula a potência do primeiro parâmetro pelo segundo, considerando que o expoente será sempre positivo
poten::Int->Int->Int
poten n e 
    | e == 1 = n
    | e == 0 = 1
    | otherwise = n * poten n (e-1)

--2. Implemente uma função nPrimo :: Int -> Int em Haskell, que recebe um número n como parâmetro e encontre o enésimo primo.
nPrimo::Int -> Int 
nPrimo n = auxNprimo n 0 0

auxNprimo:: Int -> Int -> Int -> Int
auxNprimo pos cont tprimo 
    | cont == pos = tprimo - 1
    | primo (tprimo) == True = auxNprimo pos (cont+1) (tprimo+1)
    | otherwise = auxNprimo pos (cont) (tprimo+1) 

primo::Int -> Bool
primo n
    | n == 1 = False
    |otherwise = auxPrimo n (n-1) 

auxPrimo::Int -> Int -> Bool
auxPrimo n c
    | c == 1 = True
    | mod n c == 0 = False
    | otherwise = auxPrimo n (c-1)

--3. Construa a função fibonacciPrimo :: Int -> Int que retorna o enésimo número primo da sequência de Fibonacci.
fibPrimo::Int -> Int
fibPrimo n = auxfib 0 1 0 n

auxfib::Int->Int->Int->Int->Int
auxfib prim segu conT pos
    | conT == pos = segu
    | (primo(prim+segu)) = auxfib segu (prim+segu) (conT+1) pos
    | otherwise = auxfib segu (prim+segu) conT pos

--4. Implemente, em Haskell, as funções abaixo:
--a. Função que calcula o resto da divisão de dois números inteiros positivos.
doisInteiros :: Int -> Int -> Int
doisInteiros n m = mod n 2 + mod m 2

--b. Função que calcula a divisão inteira entre dois números inteiros positivos.
doisNume:: Int -> Int -> Int
doisNume n m = div n 2 + div m 2

--c. Função que calcula o máximo divisor comum entre dois números inteiros positivos.
mdc::Int->Int->Int
mdc n m
   | n < m = auxmdc n m n 1
   | otherwise  = auxmdc n m m 1

auxmdc::Int->Int->Int->Int->Int
auxmdc n m dic mult
   | dic == 1 = mult
   | mod n dic == 0 && mod m dic == 0 && primo dic == True = auxmdc n m (dic-1) (dic*mult)
   | otherwise = auxmdc n m (dic-1) mult

--d. Função que calcula o mínimo múltiplo comum entre dois números inteiros positivos.
mmc::Int -> Int -> Int
mmc n m = auxmmc n m 2 1

auxmmc::Int->Int->Int->Int->Int
auxmmc n m increm multi
   | n == 1 && m == 1 = multi
   | mod n increm == 0 && mod m increm == 0 = auxmmc (div n increm) (div m increm) increm (increm*multi)
   | mod n increm /= 0 && mod m increm == 0 = auxmmc n (div m increm) increm (increm*multi)
   | mod n increm == 0 && mod m increm /= 0 = auxmmc (div n increm) m increm (increm*multi)
   | otherwise = auxmmc n m (increm+1) multi

{-5. Dado um número natural n > 0, n é dito perfeito se a soma de seus divisores, incluindo o
número 1, é igual ao próprio n. O primeiro número natural perfeito é o número 6, porque 6 = 1 +
2 + 3. Defina uma função ehPerfeito(n) que informe se n é, ou não, um número perfeito.-}
nuperfe::Int->Bool
nuperfe n
   | n <= 0 = False
   | n == aux n (n-1) 0 = True
   | otherwise = False

aux::Int -> Int -> Int -> Int
aux n decre soma
   | decre == 0 = soma
   | mod n (decre) == 0 = aux n (decre-1) (decre+soma)
   | otherwise = aux n (decre-1) soma