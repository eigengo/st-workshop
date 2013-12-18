#Randomness
In the collections, we used Scala to create collections containing some _random_ values! The problem with random values is that they are not pure; a function that returns random anything cannot be pure, because its result depends on something other than its arguments.

And that is a big boo-boo for pure FP, like Haskell. However, to be useful, even pure FP languages need to be able to deal with side-effects. Luckily, we have monads to help us with that; and in particular, the IO monad.

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

We can now write functions that are similar to the Scala ones. For example, to partition the people by ages is simply:


```haskell
  ages :: [Person] -> ([Person], [Person])
  ages = partition ((> 50) . age)
```

Notice the function composition ``.`` in the equation ``(> 50) . age``. It is the familiar ``f . g``, which you can read as _f_ and then _g_; in other words, a function that takes whatever _f_ takes and returns whatever _g_ applied to whatever _f_ returns. The type of ``(> 50)`` is (nearly) ``Int -> Bool``; the ``Int`` here means the person's age. We want to say take ``age``, and apply whatever that returns to ``(> 50)``. In Scala syntax, this would be

```scala
def age(p: Person): Int = ...
def gt(x: Int)(value: Int) = x > value
def over50(p: Person) = (gt(50) andThen age)(p)
//
val over50p = (gt(50) andThen age)
```

This is a bit clunky, and so we don't usually compose functions like this in Scala. However, in Haskell, the situation is completely different; composition ahoy!

---

Onwards, let's implement some of the remaining functions we had in Scala. The average age is 

```haskell
  total :: [Person] -> Int
  total = foldl (flip $ (+) . age) 0

  average :: [Person] -> Int
  average people = (total people) `div` (length people)
```

Unfortunately, I cannot write ``average people``, because ``people`` returns ``IO [Person]``, 
not ``[Person]``. I need to use the _bind_ operation.

```haskell
class Monad a where
  ...
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
```

To "see" the average, we "unwrap" the result of ``people >>= return . average``, whose type is ``IO Int``. Nota bene that ``people >>= return . average`` only returns ``IO Int``, it does not actually compute anything. To kick-off the computation, we must unwrap the value in the ``IO`` container, for example by requesting it to be displayed.