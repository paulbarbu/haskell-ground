import System.Environment
import qualified Data.ByteString.Lazy as B
import Control.Exception
import System.IO.Error

cp :: String -> String -> IO ()
cp srcName dstName = do
    srcContents <- B.readFile srcName
    B.appendFile dstName srcContents

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn $ "OUCH! Something went bonkers "
        ++ case ioeGetFileName e of
            Just path -> path
        ++ "!"
    | otherwise = ioError e

main = do
    [src,dst] <- getArgs
    (cp src dst) `catch` handler
