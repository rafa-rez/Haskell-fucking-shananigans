-------------------------------------------------------
--         TRABALHO DE PROGRAMAÇÃO FUNCIONAL         --       
--                    ALUNOS:                        --
-- JOSÉ ACERBI ALMEIDA NETO. MATRÍCULA: 202310421    --
-- RAFAEL ALVES REZENDE SILVA. MATRÍCULA: 202310875  --
-------------------------------------------------------

--Funções para a questão 25
import Data.Char (toUpper, toLower)

-- --  --  --  --
--  Questão 1  --
--  --  --  -- -- 
unica_ocorrencia :: (Eq a) => a -> [a] -> Bool
unica_ocorrencia _ [] = False
unica_ocorrencia  numero (cabeca:resto) 
    | cabeca == numero = True
    | otherwise = unica_ocorrencia  numero resto

-- --  --  --  --
--  Questão 4  --
--  --  --  -- -- 
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove elemento (cabeca:resto)
 | elemento == cabeca = resto
 | otherwise = cabeca : remove elemento resto

-- --  --  --  --
--  Questão 7  --
--  --  --  -- -- 
maiores :: Integer -> [Integer] -> [Integer]
maiores _ [] = []
maiores elemento (cabeca:resto)
  | cabeca > elemento = cabeca : maiores elemento resto
  | otherwise = maiores elemento resto

-- --  --  --  --
--  Questão 10  --
--  --  --  -- -- 
divide :: [Integer] -> Integer -> ([Integer],[Integer])
divide [] _ = ([],[])
divide original 0 = ([],original)
divide (cabeca:resto) elemento = (cabeca:primeiro, original)
  where (primeiro,original) = divide resto (elemento-1)

--  --  --  --  --
--  Questão 13  --
--  --  --  --  -- 
uniao :: Eq t => [t] -> [t] -> [t]
uniao listaA [] = listaA
uniao [] listaB = listaB
uniao listaA (cabecaB:restoB)
  | (unica_ocorrencia cabecaB listaA) == True = uniao listaA restoB
  | otherwise = uniao (listaA ++ [cabecaB]) restoB

--  --  --  --  --
--  Questão 16  --
--  --  --  --  -- 
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = m : sequencia (n - 1) (m + 1)

--  --  --  --  --
--  Questão 19  --
--  --  --  --  -- 
ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (cabeca:resto) = ordena [y | y <- resto, y <= cabeca] ++ [cabeca] ++ ordena [y | y <- resto, y > cabeca]

--  --  --  --  --
--  Questão 22  --
--  --  --  --  --
rodarEsquerda :: Int -> [lista] -> [lista]
rodarEsquerda 0 lista = lista
rodarEsquerda _ [] = []
rodarEsquerda elemento (cabeca:resto) = rodarEsquerda (elemento-1) (resto ++ [cabeca]) 

--  --  --  --  --
--  Questão 25  --
--  --  --  --  --
primeiras_maiusculas :: String -> String
primeiras_maiusculas str = capitalizaPalavras str True

capitalizaPalavras :: String -> Bool -> String
capitalizaPalavras [] _ = []
capitalizaPalavras (cabeca:resto) True = toUpper cabeca : capitalizaPalavras resto False
capitalizaPalavras (cabeca:resto) False
    | cabeca == ' ' = cabeca : capitalizaPalavras resto True
    | otherwise = toLower cabeca : capitalizaPalavras resto False

--  --  --  --  --
--  Questão 28  --
--  --  --  --  --
mediana :: (Ord a, Fractional a) => [a] -> a
mediana [] = 0
mediana lista = medianaAux lista 0 (comprimento lista - 1)

medianaAux :: (Ord a, Fractional a) => [a] -> Int -> Int -> a
medianaAux lista inicio fim
  | inicio == fim = pegaElemento lista inicio
  | inicio + 1 == fim = (pegaElemento lista inicio + pegaElemento lista fim) / 2
  | otherwise = medianaAux lista (inicio + 1) (fim - 1)

comprimento :: [a] -> Int
comprimento [] = 0
comprimento (_:resto) = 1 + comprimento resto

pegaElemento :: [a] -> Int -> a
pegaElemento (cabeca:_) 0 = cabeca
pegaElemento (_:resto) elemento = pegaElemento resto (elemento-1)

--  --  --  --  --
--  Questão 31  --
--  --  --  --  --
palindromo :: String -> Bool
palindromo str = ehPalindromo str 0 (comprimento str - 1)

ehPalindromo :: String -> Int -> Int -> Bool
ehPalindromo str i j
    | i >= j = True
    | charAt str i /= charAt str j = False
    | otherwise = ehPalindromo str (i + 1) (j - 1)

charAt :: String -> Int -> Char
charAt (cabeca:resto) 0 = cabeca
charAt (cabeca:resto) n = charAt resto (n - 1)

--  --  --  --  --
--  Questão 34  --
--  --  --  --  --
bolha :: [Int] -> [Int]
bolha xs = bolhaAux xs (comprimento xs)

bolhaAux :: [Int] -> Int -> [Int]
bolhaAux xs 0 = xs
bolhaAux xs n = bolhaAux (bolhaPassada xs) (n - 1)

bolhaPassada :: [Int] -> [Int]
bolhaPassada [] = []
bolhaPassada [x] = [x]
bolhaPassada (x:y:xs)
    | x > y     = y : bolhaPassada (x:xs)
    | otherwise = x : bolhaPassada (y:xs)
