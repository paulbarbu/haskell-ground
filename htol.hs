import System.Environment(getArgs)
import System.IO

data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show, Enum)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

optimalPath :: RoadSystem -> (Path, Int)
optimalPath roadSystem = if costA < costB then (reverse pathA, costA) else (reverse pathB, costB)
   where
   (pathA, pathB, costA, costB) = foldl (flip roadStep) ([],[], 0, 0) roadSystem

roadStep :: Section -> (Path, Path, Int, Int) -> (Path, Path, Int, Int)
roadStep (Section a b c) (pathA, pathB, costA, costB) =
    -- maybe I don't need to keep both paths, just the shortest one at all times, investigate!
    let forwardCostA = a + costA
        forwardCostB = b + costB

        crossCostA = b + c + costB
        crossCostB = a + c + costA

        (newPathA, newCostA) = if forwardCostA < crossCostA then
                    ((A, a):pathA, forwardCostA)
                   else
                    ((C, c):(B, b):pathB, crossCostA)
        (newPathB, newCostB) = if forwardCostB < crossCostB then
                    ((B, b):pathB, forwardCostB)
                   else
                    ((C, c):(A, a):pathA, crossCostB)
    in (newPathA, newPathB, newCostA, newCostB)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = left : splitEvery n right
    where (left,right) = splitAt n xs

main :: IO ()
main = do
    [file] <- getArgs -- validation
    contents <- readFile file -- validation
    let sections = splitEvery 3 . map read $ lines contents
        roadSystem = map (\[a,b,c] -> Section a b c) sections

    putStrLn . show $ optimalPath roadSystem
