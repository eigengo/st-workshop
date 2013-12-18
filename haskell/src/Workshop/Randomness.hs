module Workshop.Randomness(Person(..), people, ages, total, average) where

  import Data.List
  import System.Random
  import Control.Monad

  data Person = Person { firstName :: String
                       , lastName :: String
                       , age :: Int } deriving (Show)

  person :: (RandomGen g) => g -> IO Person
  person gen = do
    firstName <- randomString
    lastName  <- randomString
    age       <- randomInt 101
    return $ Person firstName lastName age 
    where
      randomString = do
        len <- randomInt 10
        return $ take len (randomRs ('a', 'z') gen)
      randomInt max = (randomIO :: IO Int) >>= return . (`mod` max)

  people :: IO [Person]
  people = do
    gen <- getStdGen
    replicateM 20000 (person gen)

  ages :: [Person] -> ([Person], [Person])
  ages = partition ((> 50) . age)

  total :: [Person] -> Int
  total = foldl (flip $ (+) . age) 0

  average :: [Person] -> Int
  average people = (total people) `div` (length people)