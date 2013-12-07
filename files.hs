import System.IO

withFile' :: String -> IOMode -> (Handle -> IO r) -> IO r
withFile' path mode f = do
    handle <- openFile path mode
    contents <- f handle
    hClose handle
    return contents

main = -- do
    {-handle <- openFile "haiku.txt" ReadMode-}
    {-contents <- hGetContents handle-}
    {-putStr contents-}
    {-hClose handle-}
    withFile' "haiku.txt" ReadMode (\hnd -> do
                                    contents <- hGetContents hnd
                                    putStr contents)
