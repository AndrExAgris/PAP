dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir func acumula [] =
  acumula
dobrar_dir func acumula (x:xs) =
  x `func` dobrar_dir func acumula xs