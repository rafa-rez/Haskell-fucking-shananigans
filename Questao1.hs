unica_ocorrencia  :: (Eq a) => a -> [a] -> Bool
unica_ocorrencia _ [] = False
unica_ocorrencia  numero (atual:restante) 
    | atual == numero = True
    | otherwise = unica_ocorrencia  numero restante