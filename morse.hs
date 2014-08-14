import Data.List.Split

dec' :: String -> Char
dec' x = head $ foldl f " eisuarwtndkmgo" x

f :: String -> Char -> String
f (_:acc) x | x=='.' = fstHalf
            | x=='-' = sndHalf
            where
              half = length acc `div` 2
              (fstHalf, sndHalf) = splitAt half acc

decWord' :: String -> String
decWord' x = map dec' $ splitOn "/" x