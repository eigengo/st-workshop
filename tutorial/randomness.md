#Randomness
In the collections, we used Scala to create collections containing some _random_ values! The problem with
random values is that they are not pure; a function that returns random anything cannot be pure, because
its result depends on something other than its arguments.

And that is a big boo-boo for pure FP, like Haskell. However, to be useful, even pure FP languages need 
to be able to deal with side-effects. Luckily, we have monads to help us with that; and in particular,
the IO monad.

So let's create a person record, and then a way to generate said person.

```haskell
module Workshop.Collections where

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
    age       <- randomInt 100
    return $ Person firstName lastName age 
    where
      randomString = do
        len <- randomInt 10
        return $ take len (randomRs ('a', 'z') gen)
      randomInt max = (randomIO :: IO Int) >>= return . (`mod` max)
```

Once we are able to generate one random person, we can surely _replicate_ the generating process over 
a number of random persons.

```haskell
  people :: IO [Person]
  people = do
    gen <- getStdGen
    replicateM 100 (person gen)
```

We can now write functions that are similar to the Scala ones. For example, to partition the people by
ages is simply:


```haskell
  ages :: [Person] -> ([Person], [Person])
  ages = partition ((> 50) . age)
```

The average age is 

```haskell
  total :: [Person] -> Int
  total = foldl (flip $ (+) . age) 0

  average :: [Person] -> Int
  average people = (total people) `div` (length people)
```

Unfortunately, I cannot write ``average people``, because ``people`` return ``IO [Person]``, 
not ``[Person]``. I need to use the _bind_ operation.

```haskell
class Monad a where
  ...
  (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

To "see" the average, we run ``people >>= return . average``, whose type is ``IO [Person] -> (Person -> IO Int) -> IO Int``.