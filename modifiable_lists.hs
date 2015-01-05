import Debug.Trace

data Expr = One Int | List [Expr]
    deriving (Show)

type Env = [(String, Expr)]

replace :: Int -> Expr -> [Expr] -> [Expr]
replace i val list = a ++ [val] ++ b where (a, _:b) = splitAt i list

assign :: Env -> String -> Expr -> Env
assign env name val = (name, val):env

replaceNamedIndex :: Env -> String -> [Int] -> Expr -> Env
replaceNamedIndex env name [i] val = case lookup name env of
    Nothing -> trace ("not found " ++ name) env
    Just (List l) -> (name, List $ replace i val l):env
replaceNamedIndex env name (i:is) val = case lookup name env of
    Nothing -> trace ("not found " ++ name) env
    Just (List l) -> (name, List $ replace i (List $ replaceValueIndex (l !! i) is val) l):env

replaceValueIndex :: Expr -> [Int] -> Expr -> [Expr]
replaceValueIndex (List list) [i] val = replace i val list
replaceValueIndex (List list) (i:is) val = replace i (List $ replaceValueIndex (list !! i) is val) list

main = let env = assign [] "a" $ List [One 1, List [One 2, One 3], One 4]
    in do
        print $ replaceNamedIndex env "a" [1] $ One 5 -- {1, 5, 4}
        print $ replaceNamedIndex env "a" [1] $ List [One 5, One 6] -- {1, {5, 6}, 4}
        print $ replaceNamedIndex env "a" [1, 0] $ List [One 5, One 6] -- {1, {{5, 6}, 3}, 4}
