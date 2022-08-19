-- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada
soma1 :: Int -> Int
soma1 soma = soma + 1

-- 2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo

sempre :: v -> Int
sempre v = 0

-- 3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.

treco :: Double -> Double -> Double -> Double
treco valor1 valor2 valor3 = (valor1 + valor2) * valor3

-- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.

resto :: Int -> Int -> Int
resto valor1 valor2 = valor1 `mod` valor2

-- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.

precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior v1 v2 v3 v4 = maximum[v1, v2, v3, v4]

-- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar. 

impar :: Int -> Int -> Bool
impar x y = mod (x*y) 2 == 1

-- 7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.

par :: (Int, Int) -> Int
par (x, y) = x + y

-- 8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦2 +𝑧.

equacao :: Double -> Double -> Double -> Double
equacao x y z = x**2 + y/2 + z

{- 
9. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico. 
-}

diagnostico :: Double -> String
diagnostico pesoAluno
  | imc < 17.00 = "Muito abaixo do peso"
  | imc < 18.49 = "Abaixo do peso"
  | imc < 24.99 = "Peso normal"
  | imc < 29.99 = "Sobrepeso"
  | imc < 34.99 = "Obesidade leve"
  | imc < 39.99 = "Obesidade severa"
  | otherwise = "Obesidade morbida"
  where imc = pesoAluno / 1.94 ** 2

{- 
10 Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
      𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. 
-}
bissexto :: Int -> Bool
bissexto ano = mod ano 4 == 0 && mod ano 100 /= 0 || mod ano 400 == 0

main = do
  putStrLn "Questao 1:"
  print(soma1 10)
  putStrLn $ "Func. 1: entrada: 1; resultado: " ++ show (soma1 1)
  putStrLn $ "Func. 2: entrada: OI; resultado: " ++ show (sempre "OI")
  putStrLn $ "Func. 2: entrada: 3.2; resultado: " ++ show (sempre 3.2)
  putStrLn $ "Func. 2: entrada: 4; resultado: " ++ show (sempre 4)
  putStrLn $ "Func. 3: entrada: 2.5 3.5 2.0; resultado: " ++ show (treco 2.5 3.5 2.0)
  putStrLn $ "Func. 4: entrada: 5 2; resultado: " ++ show (resto 5 2)
  putStrLn $ "Func. 5: entrada: 10.0 3.3 14.1 5.9; resultado: " ++ show (precoMaior 10.0 3.3 14.1 5.9)
  putStrLn $ "Func. 6: entrada: 2 5; resultado: " ++ show (impar 2 5)
  putStrLn $ "Func. 6: entrada: 3 5; resultado: " ++ show (impar 3 5)
  putStrLn $ "Func. 7: entrada: par (4, 6); resultado: " ++ show (par (4, 6))
  putStrLn $ "Func. 8: entrada: 2 3 4; resultado: " ++ show (equacao 2 3 4)
  putStrLn $ "Func. 9: entrada: 45.0; resultado: " ++ show (diagnostico 45.0)
  putStrLn $ "Func. 9: entrada: 67.0; resultado: " ++ show (diagnostico 67.0)
  putStrLn $ "Func. 9: entrada: 85.0; resultado: " ++ show (diagnostico 85.0)
  putStrLn $ "Func. 9: entrada: 100.0; resultado: " ++ show (diagnostico 100.0)
  putStrLn $ "Func. 9: entrada: 120.0; resultado: " ++ show (soma1 1)
  putStrLn $ "Func. 10: entrada: 1997; resultado: " ++ show (bissexto 1997)
  putStrLn $ "Func. 10: entrada: 1900; resultado: " ++ show (bissexto 1900)
  putStrLn $ "Func. 10: entrada: 2000; resultado: " ++ show (bissexto 2000)
  

