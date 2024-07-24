
-- Remove - Q4
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
 | x == y      = ys
 | otherwise = y : remove x ys

-- Maiores Q 7
maiores :: Ord a => Int -> [a] -> [a]
maiores n xs = aux n [] xs
  where
    aux 0 acc _ = reverse acc
    aux _ acc [] = reverse acc
    aux n acc (x:xs)
      | length acc < n = aux n (x:acc) xs
      | otherwise = let (minAcc, restAcc) = findMinAndRest acc
                    in if x > minAcc
                       then aux n (x:restAcc) xs
                       else aux n acc xs
    findMinAndRest [] = error "Empty list"
    findMinAndRest (y:ys) = go y [] ys
      where
        go m acc [] = (m, acc)
        go m acc (z:zs)
          | z < m     = go z (m:acc) zs
          | otherwise = go m (z:acc) zs

-- Função principal para testar a função remove
main :: IO ()
main = do
  let lista = [1, 2, 3, 4, 2, 5, 9 ,9]
  let elemento = 2
  print $ remove elemento lista  -- Output: [1, 3, 4, 2, 5]
  print $ maiores 3 lista        -- Output: [5, 8, 7]
