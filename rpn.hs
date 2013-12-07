solveRPN :: String -> Float
solveRPN =  head . foldl parseItem [] . words

parseItem :: (Num a, Read a, Floating a) => [a] -> String -> [a]
parseItem (x:y:xs) "*" = x*y :xs
parseItem (x:y:xs) "/" = y/x :xs -- x=0
parseItem (x:y:xs) "+" = x+y :xs
parseItem (x:y:xs) "-" = y-x :xs
parseItem (x:y:xs) "^" = y**x :xs -- y=0
parseItem (x:xs) "log" = log x :xs -- x=0
parseItem xs n = (read n):xs
