import Control.Monad.Trans.State
import System.Random
-- import Control.Monad

rollDice :: State StdGen Int
rollDice = do
    generator <- get
    let (x, s) = randomR (1,6) generator
    put s
    return x

--twice :: State StdGen (Int,Int)
--twice = liftM2 (,) rollDice rollDice

main :: IO ()
main = putStrLn . show $ evalState rollDice (mkStdGen 42)
