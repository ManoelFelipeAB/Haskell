{-1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr
devolva o fatorial de n-}
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1..n]

{-2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de
números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos
inteiros listados.-}
quadradoReal :: [Double] -> [Double]
quadradoReal lista = map (^2) lista

{-3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de
palavras e devolve uma lista com o comprimento de cada uma destas palavras.-}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras lista = map (length) lista

{-4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior
número entre 0 e 100000 que seja divisivel por 29.-}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = last(filter (\n -> ((n `mod` 29)==0))[0..100000])

{-5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um
inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.-}
maiorMultiploDe :: Int -> Int
maiorMultiploDe multiplo = last(filter (\n -> ((n `mod` multiplo)==0))[0..100000])

{- 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva
a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De
tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 + 2^2 + 3^2 + 4^2. . . +𝑛^2.-}
somaQuadrados :: Int -> Int
somaQuadrados n = foldr (\x y -> x*x + y) 0 [1..n]

{-7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o
comprimento (cardinalidade) de uma lista dada. -}
comprimento :: [lista] -> Int
comprimento lista = foldl (\lista x -> lista + 1) 0 lista
{-8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso
das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada
uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.-}

-- flip
flipPreludeA :: Double -> Double -> Double 
flipPreludeA x y = flip (/) x y 

flipPreludeB :: Double -> Double -> Bool 
flipPreludeB x y = flip (>) x y
-- ord


-- max
maxPrelude :: Int -> Int -> Int
maxPrelude x y = max x y
-- min
minPrelude :: Int -> Int -> Int
minPrelude x y = min x y
-- curry
curryPreludeA :: Int ->  Int -> Int
curryPreludeA  = curry (\ (x,y) -> 2*x+y)

curryPreludeB :: Int ->  Int -> Int
curryPreludeB = curry (\ (x,y) -> (x+y)^2)
-- uncurry
uncurryPreludeA:: (Int,  Int) -> Int
uncurryPreludeA = uncurry (*)

uncurryPreludeB:: (Double, Double) -> Double
uncurryPreludeB = uncurry (/)
main = do
  putStrLn $ "Func. 1: entrada: 7; resultado: " ++ show (fatorialn 7)
  putStrLn $ "Func. 2: entrada: [1,2,3,4, -2]; resultado: " ++ show (quadradoReal [1,2,3,4, -2])
  putStrLn $ "Func. 3: entrada: ['haskell', 'funcional', 'pf']; resultado: " ++ show (comprimentoPalavras ["haskell", "funcional", "pf"])
  putStrLn $ "Func. 4: entrada: Não há entrada; resultado: " ++ show (maiorMultiploDe29)
  putStrLn $ "Func. 5: entrada: 3; resultado: " ++ show (maiorMultiploDe 3)
  putStrLn $ "Func. 6: entrada: 2; resultado: " ++ show (somaQuadrados 2)
  putStrLn $ "Func. 7: entrada: [1,3,5,7,9,11,13,15]; resultado: " ++ show (comprimento [1,3,5,7,9,11,13,15])
  putStrLn $ "Func. 8 flipPreludeA: entrada: 3 5; resultado: " ++ show (flipPreludeA 3 5)
  putStrLn $ "Func. 8 flipPreludeB: entrada: 3 5; resultado: " ++ show (flipPreludeB 3 5)
  putStrLn $ "Func. 8 maxPrelude: entrada: 2 4; resultado: " ++ show (maxPrelude 2 4)
  putStrLn $ "Func. 8 maxPrelude: entrada: 2 12; resultado: " ++ show (maxPrelude 2 12)
  putStrLn $ "Func. 8 minPrelude: entrada: 2 4; resultado: " ++ show (minPrelude 2 4)
  putStrLn $ "Func. 8 minPrelude: entrada: 2 12; resultado: " ++ show (minPrelude 2 12)
  putStrLn $ "Func. 8 curryPreludeA: entrada: 3 5; resultado: " ++ show (curryPreludeA 3 5)
  putStrLn $ "Func. 8 curryPreludeB: entrada: 3 5; resultado: " ++ show (curryPreludeB 3 5)
  putStrLn $ "Func. 8 uncurryPreludeA: entrada: (3,2); resultado: " ++ show (uncurryPreludeA (3,2))
  putStrLn $ "Func. 8 uncurryPreludeB: entrada: (3,2); resultado: " ++ show (uncurryPreludeB (3,2))
  
  
