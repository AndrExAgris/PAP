{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- funçoes da rova ou outros aux que usei na lista

ultimoElemento :: [a] -> a
ultimoElemento [x] =
    x
ultimoElemento (x:xs) =
    ultimoElemento xs

-- roubei da prova
verificaImpar :: Int -> Bool
verificaImpar n = (n `mod` 2) == 1

tamanho :: [a] -> Int
tamanho [] =
    0
tamanho (x:xs) =
    1 + tamanho xs

decaptaDupla :: (a,b) -> a
decaptaDupla (a,b) =
    a


-- questão 1
concatena :: [a] -> [a] -> [a]
concatena [] [] =
    []
concatena [] ys =
    ys
concatena xs [] =
    xs
concatena (x:xs) ys =
    x: concatena xs ys


-- questão 2
pertence :: Eq a => a -> [a] -> Bool
pertence n [] =
    False
pertence n (x:xs) =
    (x == n) ||
 pertence n xs


-- questão 3
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] [] =
    []
intersecao xs [] =
    []
intersecao [] ys =
    []
intersecao (x:xs) ys =
    if pertence x ys then
        x : intersecao xs ys
    else
        intersecao xs ys


-- questão 4
inverso :: [a] -> [a]
inverso [] =
    []
inverso xs =
    ultimoElemento xs : inverso (primeiros (tamanho xs -1) xs)


-- questão 5
primeiros :: Int -> [a] -> [a]
primeiros 0 xs =
    []
primeiros n (x:xs) =
    x : primeiros (n-1) xs


-- questão 6
ultimos :: Int -> [a] -> [a]
ultimos 0 xs =
    []
ultimos n xs =
    ultimoElemento xs : ultimos (n-1) (init xs)
-- resposta invertida, corrigir antes de enviar


-- questão 7
binParaInt :: String -> Int
binParaInt [] =
    0
binParaInt ('0':xs) =
    binParaInt xs
binParaInt ('1':xs) =
    2^ tamanho xs + binParaInt xs


-- questão 8
intParaBin :: Int -> String
intParaBin 0 =
    "0"
intParaBin 1 =
    "1"
intParaBin n =
    if mod n 2 == 0 then
        concatena (intParaBin (div n 2) ) "0"
    else
        concatena (intParaBin (div n 2) ) "1"


-- questão 9
menorValor :: Ord a => [a] -> a
menorValor [] =
    error"lista vazia"
menorValor [x] =
    x
menorValor (x:xs) =
    if x < menorValor xs then
        x
    else
        menorValor xs


-- questão 10
removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro [] n =
    []
removerPrimeiro (x:xs) n =
    if x == n then
        xs
    else
        x : removerPrimeiro xs n


-- questão 11
ordena :: Ord a => [a] -> [a]
ordena [] =
    []
ordena xs =
    menorValor xs : ordena (removerPrimeiro xs (menorValor xs))


-- questão 12
dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f acc [] =
  acc
dobrar_dir f acc (x:xs) =
  x `f` dobrar_dir f acc xs


-- questão 13
dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f acc [] =
  acc
dobrar_esq f acc (x:xs) =
  dobrar_esq f (acc `f` x) xs


-- questão 14
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] =
    []
filtrar f (x:xs) =
    if f x then
        x : filtrar f xs
    else
        filtrar f xs

-- questão 15
impares :: [Int] -> [Int]
impares [] =
    []
impares xs =
    filtrar verificaImpar xs

-- questão 16
mapear :: (a -> b) -> [a] -> [b]
mapear f [] =
    []
mapear f (x:xs) =
    f x : mapear f xs


-- questão 17
primeirosDuplas :: [(a, b)] -> [a]
primeirosDuplas [] =
    []
primeirosDuplas xs =
    mapear decaptaDupla xs
-- só funciona com duplas de int, resolver isso


-- questão 18
todos :: [Bool] -> Bool
todos = dobrar_dir (==) True

-- questão 19   
data Tree a = Leaf a    
            | Branch (Tree a) (Tree a)

