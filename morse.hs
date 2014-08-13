decWith (x:_)  [] = x
decWith (_:tree) (x:xs) | x=='.' = decWith fstHalf xs
                  | x=='-' = decWith sndHalf xs
--                  | otherwise = error $ "Trovati simboli diversi da \".\" o \"-\"!"
                  where
                      half = length tree `div` 2
                      (fstHalf, sndHalf) = splitAt half tree
--decWith _ _ = error "La lettera è troppo lunga!"

dec = decWith "_eisuarwtndkmgo" -- per ora solo le lettere formate da max 3 simboli

decWord [] = ""
decWord "/" = ""
decWord xs | '/' `elem` xs = let (letter, (_:word)) = break (=='/') xs in
                               dec letter : decWord word
           | otherwise = [dec xs]
