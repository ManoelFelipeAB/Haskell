{- 1. Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando
Haskell. -}
fibonacci :: Int -> [Int]
fibonacci 1 = [0]
fibonacci 2 = [0,1]
fibonacci x = (fibonacci (x-1)) ++[last (fibonacci (x -1)) +  last (fibonacci (x -2))]

{-2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor
Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este
algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor
absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva
uma função para o cálculo do MDC entre dois números inteiros positivos, usando o
algoritmo de Euclides conforme apresentado aqui, utilizando Haskell. -}
mdc :: Int -> Int -> Int
mdc a b 
    | b == 0 = (abs a) 
    | otherwise = mdc b (a `mod` b)

{- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos
deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e
recursividade.-}
somaDosDigitos :: Int -> Int
somaDosDigitos 0 = 0
somaDosDigitos n = (n `mod` 10) + (somaDosDigitos (div n 10))

{-4. Escreva uma função que devolva a soma de todos os números menores que 10000 que
sejam múltiplos de 3 ou 5.-}
somaMultiplos :: Int -> Int
somaMultiplos 0 = 0
somaMultiplos n
  | n == 0 = 0
  | (n `mod` 3 == 0) = n + somaMultiplos (n-1)
  | (n `mod` 5 == 0) = n + somaMultiplos (n-1)
  | otherwise = somaMultiplos (n-1)

{-5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a
soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.-}

somaDosQuadrados :: [Int] -> Int
somaDosQuadrados n = if n == []
                        then 0 else (head n)^2 + somaDosQuadrados(tail n)
quadradoDasSomas :: [Int] -> Int
quadradoDasSomas n = if n == []
                        then 0 else sum(n)^2

diferenca :: [Int] -> Int
diferenca n = somaDosQuadrados(n) - quadradoDasSomas(n)

{- 6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma 
função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números 
primos menores que um determinado inteiro dado.-}
aux :: Int -> [Int] -> [Int]
aux n [] = []
aux n lista = if (head lista) `mod` n == 0 then aux n (tail lista) else [head lista] ++ aux n (tail lista) 

eulerSieve :: [Int] -> [Int]
eulerSieve [] = []
eulerSieve lista = [head lista] ++ eulerSieve(aux (head lista) (tail lista))

{-7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva 
todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores 
que um inteiro dado. -}
seqLucas :: Int -> [Int]
seqLucas 0 = [2]
seqLucas 1 = [2] ++ [1]
seqLucas x = (seqLucas (x-1)) ++ [((last(seqLucas (x-1))) + (last(seqLucas (x-2))))]

{-8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] 
devolva [3,2,1]. -}
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario lista = (aoContrario (tail lista)) ++ [head lista]
{-9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o 
produto destes valores sem usar o operador de multiplicação -}
somaRecursiva :: Int -> Int -> Int
somaRecursiva v1 0 = 0
somaRecursiva v1 v2 = v1 + (somaRecursiva v1 (v2-1))
{-10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o 
comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule 
o comprimento de uma lista -}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento lista = 1 + (comprimento (init(lista)))
main = do
  
  putStrLn $ "Func. 1: entrada: 10; resultado: " ++ show (fibonacci 10)
  putStrLn $ "Func. 2: entrada: 10 5; resultado: " ++ show (mdc 10 5)
  putStrLn $ "Func. 3: entrada: 1234; resultado: " ++ show (somaDosDigitos 1234)
  putStrLn $ "Func. 4: entrada: 9999; resultado: " ++ show (somaMultiplos 9999)
  putStrLn $ "Func. 5: entrada: [1, 2, 3, 4]; resultado: " ++ show (diferenca [1, 2, 3, 4])
  putStrLn $ "Func. 6: entrada: [2..40]; resultado: " ++ show (eulerSieve [2..40])
  putStrLn $ "Func. 7: entrada: 10; resultado: " ++ show (seqLucas 10)
  putStrLn $ "Func. 8: entrada: [1,3,5,7,9,11]; resultado: " ++ show (aoContrario [1,3,5,7,9,11])
  putStrLn $ "Func. 9: entrada: 4 4; resultado: " ++ show (somaRecursiva 4 4)
  putStrLn $ "Func. 10: entrada: [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]; resultado: " ++ show (comprimento [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  