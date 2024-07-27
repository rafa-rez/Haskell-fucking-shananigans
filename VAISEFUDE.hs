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

-- Sequência começando de X números, seguindo: [y, y+1, y+2...]
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = m : sequencia (n - 1) (m + 1)

-- Ordenação de listas
ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (x:xs) = ordena [y | y <- xs, y <= x] ++ [x] ++ ordena [y | y <- xs, y > x]


-- Função principal para testar a função remove
main :: IO ()
main = do
  let lista = [1, 2, 3, 4, 2, 5, 9 ,9]

  let elemento = 2
  print $ remove elemento lista  -- Saída: [1, 3, 4, 2, 5]
  
  print $ maiores 3 lista        -- Saída: [5, 8, 7]
  
  print $ divide lista 4    -- Saída: ([1,2,3,4], [2,5,9,9])

  let lista1 = [1, 1, 1, 2]
  let lista2 = [2, 1, 2, 1, 2]
  print $ uniao lista1 lista2  -- Saída: [3, 6, 5, 7, 2, 9, 1]

  print $ sequencia 3 4  -- Saída: [4, 5, 6]

  print $ ordena [5, 2, 1, 3, 4]        -- Output: [1, 2, 3, 4, 5]

