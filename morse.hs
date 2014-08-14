import Data.List.Split

dec :: String -> Char
dec letter = head $ foldl traverseTree " eisuarwtndkmgo" letter

traverseTree :: String -> Char -> String
traverseTree (_:tree) selector | selector=='.' = fstHalf
                               | selector=='-' = sndHalf
                               where
                                 half = length tree `div` 2
                                 (fstHalf, sndHalf) = splitAt half tree

decWord :: String -> String
decWord word = map dec $ splitOn "/" word