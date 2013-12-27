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

  type Generate w = WriterT w (StateT Integer IO)

  personT :: IO a -> Generate [String] a
  personT person = do
    count <- get
    tell  ["P #" ++ show count]
    put   (count + 1)
    liftIO person

  peopleT :: (Monoid w) => Generate w a -> Generate w [a]
  peopleT = replicateM 10 

  runGenerate :: Generate w a -> IO ((a, w), Integer)
  runGenerate gen = runStateT (runWriterT gen) 0
  
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
  people = replicateM 20 person 

  ages :: [Person] -> ([Person], [Person])
  ages = partition ((> 50) . age)

  total :: [Person] -> Int
  total = foldl (flip $ (+) . age) 0

  average :: [Person] -> Int
  average people = (total people) `div` (length people)
