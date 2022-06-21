--1. Utilizando Expressões-ZF, implemente as listas abaixo.
--a. Ímpares entre 1 e 100
impares::[Int]
impares = [l | l <- [1,2..100], mod l 2 /= 0]
--b. Pares entre 10 e 100
pares::[Int]
pares = [l | l <- [10,11..100], mod l 2 == 0]
--c. Ímpares entre 1 e N
lista::Int->[Int]
lista n = [i | i <- lis, mod i 2 /= 0]
  where 
      lis = [1,2..n]
--d. Números entre 1 e N que são múltiplos de 3 e 5 ao mesmo tempo.
multi::Int->[Int]
multi n = [i | i <- [1,2..n], mod i 3 == 0 , mod i 5 == 0]
--e. Tuplas entre 1 e N, contendo o número e seu respectivo quadrado.
tupl::Int->[(Int,Int)]
tupl n = [(i,i*2) | i <- [1,2..n]]
--f. Tuplas com os índices de uma matriz 3x4.
matrixtq::[(Int,Int)]
matrixtq = [(x,y)|x <- [1,2..3], y <- [1,2..4]]
--g. Tuplas com os índices de uma matriz NxM.
matrixnm::Int->Int->[(Int,Int)]
matrixnm n m = [(x,y)|x <- [1,2..n], y <- [1,2..m]]
--2. Escreva uma função com a seguinte assinatura listaFibonacci :: Int->[Int] que retorna uma lista
--com os n primeiros números da sequência de Fibonacci.
fibonacci::Int -> Int
fibonacci n
    | n == 1 = 0 
    | n == 2 = 1
    | otherwise = auxfib 0 1 (n-2)

auxfib::Int->Int->Int->Int
auxfib prim segu pos
    | pos == 0 = segu
    | otherwise = auxfib segu (prim+segu) (pos-1)

listaFibonacci::Int->[Int]
listaFibonacci n = [fibonacci a | a <- [1,2..n]]
