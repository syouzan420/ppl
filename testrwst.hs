import System.Random
import Control.Monad.RWS

main :: IO ()
main = do answer <- getStdRandom (randomR (1,10))
          let maxTries = 5
          putStrLn "I'm thinking of a number between 1 and 10, can you guess it?"
          putStrLn $ "You've got " ++ show maxTries ++ " tries!"
          (guesses,logs) <- execRWST (guessSession answer) maxTries 0
          putStrLn $ "Finieshed in " ++ show guesses ++ " tries."
          putStrLn $ "Guesses: " ++ show logs

data MS = GotIt | TooLow | TooHigh | GameOver deriving (Eq,Show)

guessSession :: Int -> RWST Int [Int] Int IO ()
guessSession answer =
  do maxTries <- ask      -- Get the maxTries from Reader Env
     g <- lift readLn     -- get guess from user 
     tell [g]             -- Log guess to Writer
     modify (+1)          -- increment number of guesses in State
     tries <- get         -- Get current number of guess from State
     let mes = case (compare g answer, compare tries maxTries) of
                (EQ, _) -> [GotIt] 
                (LT,EQ) -> [TooLow,GameOver] 
                (GT,EQ) -> [TooHigh,GameOver] 
                (LT, _) -> [TooLow] 
                (GT, _) -> [TooHigh] 
     if mes==[TooLow] || mes==[TooHigh] then guessSession answer 
                                        else (lift . return) ()
