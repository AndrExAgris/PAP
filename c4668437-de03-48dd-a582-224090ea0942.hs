{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--questão 1
concatena :: [a] -> [a] -> [a]
concatena [] [] =
    []
concatena [] ys = 
    ys
concatena xs [] =
    xs
concatena (x:xs) ys =
    x: concatena xs ys

--questão 4
inverso :: [a] -> [a]
inverso [] =
    []
inverso xs =
    last xs : inverso (init xs)

--questão 5
primeiros :: Int -> [a] -> [a]
primeiros 0 xs =
    []
primeiros n (x:xs) =
    x : primeiros (n-1) xs

--questão 6
ultimos :: Int -> [a] -> [a]
ultimos 0 xs =
    []
ultimos n xs =
    last xs : ultimos (n-1) (init xs)

--questão 9
menorValor :: Ord a => [a] -> a
menorValor [] =
    error"ERROU"
menorValor [x] =
    x
menorValor (x:xs) =
    if x < menorValor xs then
        x
    else
        menorValor xs

--exercicio 11
ordena :: Ord a => [a] -> [a]
ordena [] =
    []
ordena [x] =
    x : []
ordena (x:xs) =
    if x < menorValor xs then 
        x : ordena xs
    else
        ordena xs
