import Data.Char
import Control.Monad

main = do
    {-putStrLn "Can I haz inputs?!"-}
    lines <- getContents
    putStr $ map toUpper lines

