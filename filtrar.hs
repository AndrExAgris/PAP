verificaImpar :: Int -> Bool
verificaImpar n = (n `mod` 2) == 1

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] =
    []
filtrar f (x:xs) =
    if f x then
        x : filtrar f xs
    else
        filtrar f xs
