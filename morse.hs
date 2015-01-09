import Data.List.Split
import Data.List (intercalate)

dec :: String -> Char
dec letter = head $ foldl traverseTree " eishvuf?arl?wpjtndbxkcymgzqo??" letter

traverseTree :: String -> Char -> String
traverseTree (_:tree) selector | selector=='.' = fstHalf
                               | selector=='-' = sndHalf
                               where
                                 half = length tree `div` 2
                                 (fstHalf, sndHalf) = splitAt half tree

decWord :: String -> String
decWord word = map dec $ splitOn "/" word


encWith :: String -> Char -> String
encWith (x:tree) letter | letter == x = ""
                        | letter `elem` fstHalf = '.':(encWith fstHalf letter)
                        | letter `elem` sndHalf = '-':(encWith sndHalf letter)
                        where
                          half = length tree `div` 2
                          (fstHalf, sndHalf) = splitAt half tree

enc :: Char -> String
enc = encWith " eishvuf?arl?wpjtndbxkcymgzqo??"

encWord :: String -> String
encWord word = intercalate "/" $ map enc word
