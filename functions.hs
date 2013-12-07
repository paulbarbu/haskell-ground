import Data.List

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
    if x > 100
        then x
    else
        doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

boomBangs xs = [if x < 10 then "boom!" else "bang!" | x <- xs, odd x]


removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
-- removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
removeUppercase st = [c | c <- st, not(c `elem` ['A'..'Z'])]
triangles = [(a,b,c) | a <- [1..10], b <- [1..a], c <- [1..b], b^2 + c^2==a^2, a+b+c==24]

{-length' :: (Num b) => [a] -> b
length' xs = fromIntegral(length xs)-}

lucky :: Integral a => a -> String
lucky 7 = "OMG!"
lucky x = "TRY AGAIN!"

factorial n = product [1..n]

factorial' 0 = 1
factorial' n = n * factorial'(n-1)


my_fail :: Char -> String
my_fail 'a' = "ASD"
my_fail 'b' = "BSD"

head' :: [a] -> a
head' [] = error "FUCK OFF!"
head' (x:xs) = x

length' :: Integral b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weigth height
    | bmi <= 18.5 = "Small!"
    | bmi <= 25.0 = "Ok!"
    | bmi <= 30.0 = "Pretty big!"
    | otherwise   = "Big!"
    where bmi = weigth / height^2

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ " " ++ [l]

bmiList :: (RealFloat a) => [(a, a)] -> [a]
bmiList [] = []
bmiList (x:xs) = [bmi x] ++ bmiList xs
    where bmi (weigth, height) = weigth / height ^ 2

bmiList' :: (RealFloat a) => [(a, a)] -> [a]
bmiList' xs = [bmi w h | (w, h) <- xs]
    where bmi weigth height = weigth / height^2

hashWords :: String -> [String]
hashWords text = [tail x | x <- words' text, head x == '#']

words' :: String -> [String]
words' text = [takeWhile(/= ' ') text] ++ words(dropWhile (/= ' ') text)

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty"
                                                [x] -> "a singleton"
                                                xs -> "long"

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
    where what [] = "e"
          what [x] = "s"
          what xs = "l"

max' :: (Ord a) => [a] -> a
max' [] = error "Can't compute max' on an empty list!"
max' [x] = x
max' (x:xs)
    | x > t = x
    | otherwise = t
    where t = max' xs

max'' :: (Ord a) => [a] -> a
max'' [] = error "Can't compute max'' on an empty list!"
max'' [x] = x
max'' (x:xs) = max x (max'' xs)

replicate' :: (Integral a, Ord a) => a -> b -> [b]
replicate' n el
    | n <= 0 = []
    | otherwise = el:replicate' (n-1) el

take' :: (Integral a) => a -> [b] -> [b]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

repeat' :: a -> [a]
repeat' x = x:repeat' x

{- docblocks? -}

elem' :: (Eq a) => a -> [a] -> Bool
_ `elem'` [] = False
x `elem'` (y:ys)
    | x == y = True
    | otherwise = x `elem'` ys

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
{-quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]-}
quickSort (x:xs) = quickSort (filter (<= x) xs) ++ [x] ++ quickSort (filter (> x) xs)
{-quickSort (x:xs) = let-}
    {-smaller = quickSort [y | y <- xs, y <= x]-}
    {-greater = quickSort [y | y <-xs, y > x]-}
    {-in smaller ++ [x] ++ greater-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
{-filter' f (x:xs) = [y | y <- [x], f x] ++ (filter' f xs)-}

filter' f (x:xs)
    | f x = x:tail
    | otherwise = tail
    where tail = filter' f xs

sumOdd :: (Integral a, Ord a) => a -> a
sumOdd n = sum (takeWhile (<=n) (filter odd (map (^2) [1..])))

collatzSeq :: (Integral a) => a -> [a]
collatzSeq 1 = [1]
collatzSeq n
    | odd n = n:collatzSeq (n*3 + 1)
    | even n = n:collatzSeq (n `div` 2)

numLongCollatzSeq :: Int -> Int
{-numLongCollatzSeq n = length (filter isLong (map collatzSeq [1..100]))-}
    {-where isLong xs = length xs > n-}
numLongCollatzSeq n = length (filter (\xs -> length xs > 15) (map collatzSeq [1..100]))

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldl (\acc y -> if x == y then True else acc) False xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x:acc) [] xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = foldl1 (\acc x -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> acc ++ [x]) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []

head'' :: [a] -> a
head'' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

subList :: (Eq a) => [a] -> [a] -> Bool
subList needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- Get the multiples of n
mul :: (Integral a) => a -> [a]
mul n = [x | x <- [2..], x `mod` n == 0]

{- Using the Sieve Of Eratosthenmes algorithm get all primes up to n
 -
 - This works by reducing the multiples of the numbers from the initial list
 -}
soe :: (Integral a) => a-> [a]
soe n = foldl (\acc x -> acc \\ (takeWhile (<=n) (tail . mul $ x))) [2..n] [2..n]

irc_soe :: (Integral a) => a-> [a]
irc_soe n = nubBy (\x y -> gcd x y > 1) [2..n]

nubBy' eq [] = []
nubBy' eq (x:xs) = x : nubBy' eq (filter (\ y -> not (eq x y)) xs)

asd (x:xs) = length xs

my_dict =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

lkup :: (Eq k) => k -> [(k, v)] -> Maybe v
{-lkup _ [] = Nothing-}
{-lkup key ((k,v):xs)-}
    {-| k == key = Just v-}
    {-| otherwise = lkup key xs-}

lkup key = foldl (\acc (k,v) -> if  k == key then Just v else acc) Nothing
