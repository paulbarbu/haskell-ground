import Data.Monoid

{-comp :: String -> String -> Ordering-}
{-comp xs ys-}
    {-| length xs < length ys = LT-}
    {-| length xs > length ys = GT-}
    {-| otherwise = xs `compare` ys-}

comp' :: String -> String -> Ordering
comp' x y = (length x `compare` length y) `mappend`
            noVowels x y `mappend`
            (x `compare` y)
            where noVowels x y = (length $ getVowels x) `compare`
                                 (length $ getVowels y)
                  getVowels = filter (`elem` "aeiou")
