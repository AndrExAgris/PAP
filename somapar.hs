somaPares :: Int -> Int
somaPares 0 =
    0 
somaPares n =
    if even n then
        n + somaPares (n-1)
    else
        somaPares (n-1)