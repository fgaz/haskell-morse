--decWith :: String -> String -> Either String Char
decWith [] (x:_)  = x --Right x
decWith (l:ls) (_:xs) | l=='.' = decWith ls fstHalf
                  | l=='-' = decWith ls sndHalf
--                  | otherwise = Left $ "Trovati simboli diversi da \".\" o \"-\"!"
                  where
                      half = length xs `div` 2
                      (fstHalf, sndHalf) = splitAt half xs
--decWith _ _ = Left "La lettera è troppo lunga!"

--dec :: String -> Either String Char
dec x = decWith x "_eisuarwtndkmgo" -- per ora solo le lettere formate da max 3 simboli

decWord :: String -> String
decWord [] = ""
decWord "/" = ""
decWord xs | '/' `elem` xs = let (letter, (_:word)) = break (=='/') xs in
                               dec letter : decWord word
           | otherwise = [dec xs]
--             where (letter, (_:word)) = break (==' ') xs