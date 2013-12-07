import System.IO
import System.IO.Error
import Control.Exception
import System.Directory
import System.Environment
import Data.List
import Text.Read
import Data.Maybe

{-
 - with readMaybe the first complicated case will be fromMaybe (length tasks) (readMaybe num), I think
 - http://www.haskell.org/ghc/docs/latest/html/libraries/base/Text-Read.html

hlint

http://www.haskell.org/haskellwiki/Smart_constructors
 -}
dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("view", view),
            ("remove", remove),
            ("bump", bump)
            ]

add :: [String] -> IO ()
add [file] = putStrLn "Please provide an item to add to the list!"
add (file:"":_) = putStrLn "Please provide a non-empty item to add to the list!"
add (file:item:_) = appendFile file (item ++ "\n")

view :: [String] -> IO ()
view (file:_) = withFile file ReadMode
    (\hnd -> do
        contents <- hGetContents hnd
        mapM_ putStrLn $ zipWith (\n l -> show n ++ ": " ++ l) [0..] $ lines contents)

updateFile :: FilePath -> [String] -> IO ()
updateFile fileName tasks = do
    (tempName, tempHnd) <- openTempFile "." "todo_t"
    hPutStr tempHnd $ unlines tasks
    hClose tempHnd

    removeFile fileName
    renameFile tempName fileName

remove :: [String] -> IO ()
remove [file] = putStrLn "Please provide a task number to remove from the list!"
remove (file:num:_) = do
    contents <- readFile file

    let tasks = lines contents
        n = fromMaybe (length tasks) (readMaybe num :: (Maybe Int))

        -- is there a cleaner way to say that I want n to be in a range?
        newTasks = if n >= length tasks || n < 0
            then tasks
            else delete (tasks !! n) tasks

    updateFile file newTasks

bump :: [String] -> IO ()
bump [file] = putStrLn "Please provide a number to bump in the list!"
bump (file:num:_) = do
    contents <- readFile file

    let tasks = lines contents
        user_n = fromMaybe (length tasks) (readMaybe num :: (Maybe Int))

        n = if user_n >= length tasks || user_n < 0 then 0 else user_n

        delTasks = delete (tasks !! n) tasks
        newTasks = if not . null $ tasks then (tasks !! n):delTasks else []

    updateFile file newTasks

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn $ "Inexistent file: " ++
        case ioeGetFileName e of
            Just path -> path
    | otherwise = ioError e

usage :: IO ()
usage = putStr $ "Usage:\n\
\ - ./todo add tasks.txt \"do the dishes\"\n\
\ - ./todo add tasks.txt \"broom the floor\"\n\
\ - ./todo view tasks.txt\n\
\ - ./todo bump tasks.txt 0\n\
\ - ./todo remove tasks.txt 1\n"

main = do
    argList <- getArgs

    -- if I don't get at least a command and a filename I assign "" to cmd so
    -- the lookup below will fail, any suggestion to make this more readable?
    let (cmd:args) = if length argList >= 2 then argList else ["",""]

    case lookup cmd dispatch of
        Just action -> action args `catch` handler
        Nothing -> usage
