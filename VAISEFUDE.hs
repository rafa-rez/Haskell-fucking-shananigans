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

--  --  --  --  --
--  Questão 28  --
--  --  --  --  --
mediana :: [Int] -> Float
mediana [] = 0
mediana lista = medianaAux lista 0 (contaElementos lista - 1)

medianaAux :: [Int] -> Int -> Int -> Float
medianaAux lista inicio fim
  | inicio == fim = fromIntegral (pegaElemento lista inicio)
  | inicio + 1 == fim = (fromIntegral (pegaElemento lista inicio) + fromIntegral (pegaElemento lista fim)) / 2.0
  | otherwise = medianaAux lista (inicio + 1) (fim - 1)

contaElementos :: [Int] -> Int
contaElementos [] = 0
contaElementos (_:resto) = 1 + contaElementos resto

pegaElemento :: [Int] -> Int -> Int
pegaElemento (cabeca:_) 0 = cabeca
pegaElemento (_:resto) elemento = pegaElemento resto (elemento-1)




