module Workshop.Randomness(Person(..), people, ages, total, average) where

  import Data.List
  import System.Random
  import Control.Applicative 
  import Control.Monad
  import Control.Monad.Writer
  import Control.Monad.State

  data Person = Person { firstName :: String
                       , lastName :: String
                       , age :: Int } deriving (Show)

  type Generate w s = WriterT w (StateT s IO)

  personT :: IO a -> Generate [String] Integer a
  personT person = do
    count <- get
    tell  ["P #" ++ show count]
    put   (count + 1)
    liftIO person

  peopleT :: (Monoid w) => Generate w s a -> Generate w s [a]
  peopleT = replicateM 10 

  runGenerate :: (Num s) => Generate w s a -> IO ((a, w), s)
  runGenerate gen = runStateT (runWriterT gen) (fromInteger 0)
  
  person :: IO Person
  person = do
    firstName <- randomString
    lastName  <- randomString
    age       <- randomInt 101
    return $ Person firstName lastName age 
    where
      randomString = do
        gen <- getStdGen
        len <- randomInt 10
        return $ take len (randomRs ('a', 'z') gen)
      randomInt max = (`mod` max) <$> (randomIO :: IO Int) 

  people :: IO [Person]
  people = replicateM 200 person 

  ages :: [Person] -> ([Person], [Person])
  ages = partition ((> 50) . age)

  total :: [Person] -> Int
  total = foldl (flip $ (+) . age) 0

  average :: [Person] -> Int
  average people = (total people) `div` (length people)
