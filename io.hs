import Control.Monad
import Data.Char
import System.Random

{-main = do-}
    {-putStrLn "Hello, world!"-}
    {-name <- getLine-}
    {-asd <- putStrLn ("Hi, " ++ name ++ "!")-}

{-main = do-}
    {-line <- getLine-}

    {-if null line-}
    {-then return ()-}
    {-else do-}
        {-putStrLn $ reverseWords line-}
        {-main-}

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do 
    putChar x
    putStr' xs

{-main = do-}
    {-let yeah = "yeah!"-}
        {-foo = "foobar"-}
    {-putStr' $ yeah ++ foo-}

{-main = do-}
    {-c <- getChar-}
    {-when (c /= ' ') $ do-}
        {-putChar c-}
        {-main-}

{-main = do-}
    {-colors <- forM [1..4] (\a -> do -}
        {-putStrLn "Gimmie a color plz: "-}
        {-getLine)-}

    {-mapM_ putStrLn colors-}

capsShortLines :: String -> String
capsShortLines xs = let
    allLines = lines xs
    shortLines = filter (\a -> length a <= 10) allLines
    result = unlines shortLines
    in
    map toUpper result

{-main = do-}
    {-[>contents <- getContents<]-}
    {-[>putStr $ capsShortLines contents<]-}
    {-interact capsShortLines-}

isPalindrome :: String -> String
isPalindrome = unlines . map (\a -> if a == reverse a then "pal" else "not pal") . lines

{-main = interact isPalindrome-}

coinToss :: StdGen -> (Bool, Bool, Bool)
coinToss gen = let
    (fstToss, fstGen) = random gen
    (sndToss, sndGen) = random fstGen
    (trdToss, _) = random sndGen
    in (fstToss, sndToss, trdToss)

{-main = do-}
    {-gen <- getStdGen-}
    {-putStrLn $ take 10 (randomRs ('a', 'z') gen)-}

guessNumber :: Int -> IO ()
guessNumber computer_n = do
    n <- getLine

    when (not $ null n) $ do
        let list = reads n

        if length list > 0 then do
            let [(user_n, _)] = list
            if user_n == computer_n then do

                putStrLn "You win!"
                main
            else do
                putStrLn "Try again:"
                guessNumber computer_n
        else do
            putStrLn "Try again, this time with valid input"
            guessNumber computer_n


main = do
    gen <- newStdGen
    let (computer_n, gen') = randomR (1, 5) gen :: (Int, StdGen)

    putStrLn "Guess a number: "
    guessNumber computer_n
