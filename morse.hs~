decWith :: String -> String -> Either String Char
decWith [] (x:_)  = Right x
decWith (l:ls) (_:xs) | l=='.' = decWith ls fstHalf
                  | l=='-' = decWith ls sndHalf
                  | otherwise = Left $ "Trovati simboli diversi da \".\" o \"-\"!"
                  where
                      half = length xs `div` 2
                      fstHalf = take half xs
                      sndHalf = drop half xs
decWith _ _ = Left "La lettera è troppo lunga!"

dec :: String -> Either String Char
dec x = decWith x "_eisuarwtndkmgo"