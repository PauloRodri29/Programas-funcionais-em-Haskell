--Letra A
primogemeo::Int->Int->Bool
primogemeo n m 
   | primo n && primo m == True = ehgemeo n m
   | otherwise = False

ehgemeo::Int->Int->Bool
ehgemeo n m
   | n > m && (n-m) == 2 || m > n && (m-n) == 2 = True
   | otherwise = False

primo::Int -> Bool
primo n
    | n == 1 = False
    |otherwise = auxPrimo n (n-1) 
   
auxPrimo::Int -> Int -> Bool
auxPrimo n c
    | c == 1 = True
    | mod n c == 0 = False
    | otherwise = auxPrimo n (c-1)

--Letra B
existepg::Int->Bool
existepg p = aux p (p+2) (p-2)

aux:: Int->Int->Int->Bool
aux n m q
    | primogemeo n m == True = True
    | primogemeo n q == True = True
    | otherwise = False

--Letra c
contparegem::Int->Int
contparegem n = auxcont n 3 5 0 -- valores mudados do segundo e terceiro argumento (argumento antigo 2 3)

auxcont::Int->Int->Int->Int->Int
auxcont n p s cont
    | n <= 3 = 0 --implementado nova condição
    | s >= n = cont
    | primogemeo p s == True = auxcont n (p+2) (s+2) (cont+1) -- implementada para alternar entre números ímpares
    | otherwise = auxcont n (p+2) (s+2) cont -- implementada para alternar entre números ímpares

--Letra D
somapg::Int->Int
somapg n = auxsoma n 3 5 0 -- valores mudados do segundo e terceiro argumento (argumento antigo 2 3)

auxsoma::Int->Int->Int->Int->Int
auxsoma n p q soma
    | n <= 3 = 0 -- implementado nova condição
    | q >= n = soma
    | primogemeo p q == True = auxsoma n (p+2) (q+2) (p+q+soma) -- implementada para alternar entre números ímpares
    | otherwise = auxsoma n (p+2) (q+2) soma -- implementada para alternar entre números ímpares

--1. Defina a função potencia :: Int -> Int -> Int em Haskell, que calcula a potência do primeiro parâmetro pelo segundo, considerando que o expoente será sempre positivo
poten::Int->Int->Int
poten n e 
    | e == 1 = n
    | e == 0 = 1
    | otherwise = n * poten n (e-1)

--2. Defenir a função div(x,y) que calcula a divisão inteira de x por y , com y e x números inteiros
divi::Int->Int->Int
divi dividendo divisor
   | divisor == 0 = 0
   | divisor == 1 = dividendo
   | otherwise = div dividendo divisor

--3Definir a função Mod(x,y) que calcule o resto da divisão de x e y 
resto::Int->Int->Int
resto n m
   | n < m = n
   | otherwise = mod n m