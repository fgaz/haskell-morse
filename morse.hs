--decWith :: String -> String -> Either String Char
decWith (x:_)  [] = x --Right x
decWith (_:tree) (x:xs) | x=='.' = decWith fstHalf xs
                  | x=='-' = decWith sndHalf xs
--                  | otherwise = Left $ "Trovati simboli diversi da \".\" o \"-\"!"
                  where
                      half = length tree `div` 2
                      (fstHalf, sndHalf) = splitAt half tree
--decWith _ _ = Left "La lettera è troppo lunga!"

--dec :: String -> Either String Char
dec = decWith "_eisuarwtndkmgo" -- per ora solo le lettere formate da max 3 simboli

decWord :: String -> String
decWord [] = ""
decWord "/" = ""
decWord xs | '/' `elem` xs = let (letter, (_:word)) = break (=='/') xs in
                               dec letter : decWord word
           | otherwise = [dec xs]
