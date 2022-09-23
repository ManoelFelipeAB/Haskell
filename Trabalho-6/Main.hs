{- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. -}
divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1..n], (n `mod` x) == 0]

{-2. Usando  List Comprehension  escreva  uma  função, chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada. -}
contaCaractere :: String -> Char -> Int
contaCaractere string character = length [ x| x <- string, x==character ]

{-3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. -}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [n*2 | n <- lista, n>=0]

{-4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a,b,c) | a <-[1..n], b <-[1..n], c <- [1..n], ((a^2) + (b^2)) == (c^2), c > a, c > b, b > a]


{-5. Números  perfeitos  são  aqueles  cuja  soma  dos seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n], (sum (init (divisoresden x))) == x]

{-6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lista1 lista2 = (sum [x*y | (x, y) <- zip lista1 lista2])

{-7. Usando  List Comprehension  escreva  uma  função, chamada  primeirosPrimos,  que  devolva uma lista contendo, os n primeiros números primos a partir do número 2. -}
primeirosPrimos :: Int -> [Int]
primeirosPrimos n = take n [x | x <- [2..], (length (divisoresden x))==2]

{-8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par ordenados  contendo  uma  potência  de  2  e  uma potência de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. -}
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados n = [(2^x, 3^x) | x <- [1..n]]

main = do
  putStrLn $ "Func. 1: entrada: 20; resultado: " ++ show (divisoresden 20)
  putStrLn $ "Func. 2: entrada: multidisciplinaridade; resultado: " ++ show (contaCaractere "multidisciplinaridade" 'i')
  putStrLn $ "Func. 3: entrada: [-1, 2, 4, 8, 16, 32]; resultado: " ++ show (dobroNaoNegativo [-1, 2, 4, 8, 16, 32])
  putStrLn $ "Func. 4: entrada: 20; resultado: " ++ show (pitagoras 20)
  putStrLn $ "Func. 5: entrada: 2000; resultado: " ++ show (numerosPerfeitos 2000)
  putStrLn $ "Func. 6: entrada: [1..5] [1..5]; resultado: " ++ show (produtoEscalar [1..5] [1..5])
  putStrLn $ "Func. 7: entrada: 20; resultado: " ++ show (primeirosPrimos 20)
  putStrLn $ "Func. 8: entrada: 20; resultado: " ++ show (paresOrdenados 20)