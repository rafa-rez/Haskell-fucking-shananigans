-- Definição da função remove
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y    = ys
  | otherwise = y : remove x ys

-- Função principal para testar a função remove
main :: IO ()
main = do
  let lista = [1, 2, 3, 4, 2, 5]
  let elemento = 2
  print $ remove elemento lista  -- Output: [1, 3, 4, 2, 5]
