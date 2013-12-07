h :: Int -> [String]
h n 
 | n > 1 = (ab (n-1)) ++ ["A-C"] ++ (bc (n-1))
 | otherwise = ["n>1 please"]

ab :: Int -> [String]
ab 1 = ["A-B"]
ab n = h n

bc :: Int -> [String]
bc 1 = ["B-C"]
bc n = h n

main :: IO()
main = do
    n <- getLine
    let (discs,_) = (reads n) !! 0
    putStrLn $ unwords (h discs)
