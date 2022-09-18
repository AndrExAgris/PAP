intercala :: Ord a => [a] -> [a] -> [a]
intercala [] [] =
    []
intercala [] xs =
    xs
intercala xs [] =
    xs
intercala (x:xs) (y:ys) =
    if x<y then
        x : intercala xs (y:ys)
    else
        y : intercala (x:xs) ys