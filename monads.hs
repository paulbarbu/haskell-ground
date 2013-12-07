applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe (Just x) f = f x
applyMaybe Nothing f = Nothing

myf :: Int -> Maybe Int
myf x = return (x+1)

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs((left+n) - right) < 4 = Just (left+n,right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs(left - (right+n)) < 4 = Just (left,right+n)
    | otherwise = Nothing

{-
 - How is it that we can break out of do notation (monads chain) with let
 - bindings.
 -
 - How dows the chaining affect IO monads, what's the role of the chaining.
-}
