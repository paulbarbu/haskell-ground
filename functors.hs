import System.IO
import Control.Applicative

{-instance Functor IO where-}
    {-fmap f action = do-}
        {-result <- action-}
        {-return (f result)-}

{-main :: IO ()-}
{-main = do-}
    {-line <- fmap reverse getLine-}
    {-[>let line' = reverse line<]-}
    {-putStrLn $ line-}
    {-putStrLn $ line-}

{-CMaybe a = CNothing | CJust Int a deriving (Show)-}

{-instance Functor CMaybe where-}
    {-fmap _ CNothing = CNothing-}
    {-fmap f (CJust x a) = CJust (x+1) (f a)-}


{-myAction :: IO String-}
{-myAction = do-}
    {-a <- getLine-}
    {-b <- getLine-}
    {-return $ a ++ b-}

{-myAction :: IO String-}
{-myAction = (++) <$> getLine <*> getLine-}

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (\x acc -> (:) <$> x <*> acc) (pure [])
