decWith (x:_)  [] = x
decWith (_:tree) (x:xs) | x=='.' = decWith fstHalf xs
                  | x=='-' = decWith sndHalf xs
--                  | otherwise = error $ "Trovati simboli diversi da \".\" o \"-\"!"
                  where
                      half = length tree `div` 2
                      (fstHalf, sndHalf) = splitAt half tree
--decWith _ _ = error "La lettera è troppo lunga!"

dec = decWith " eisuarwtndkmgo" -- per ora solo le lettere formate da max 3 simboli

decWord [] = ""
decWord "/" = ""
decWord xs | '/' `elem` xs = let (letter, (_:word)) = break (=='/') xs in
                               dec letter : decWord word
           | otherwise = [dec xs]


--or with foldl:

dec' :: String -> Char
dec' x = head $ foldl f "_eisuarwtndkmgo" x

f :: String -> Char -> String
f (_:acc) x | x=='.' = fstHalf
            | x=='-' = sndHalf
            where
              half = length acc `div` 2
              (fstHalf, sndHalf) = splitAt half acc