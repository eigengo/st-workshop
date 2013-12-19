#Randomness
In the collections module, we used Scala to create collections containing some _random_ values! The problem with random values is that they are not pure; a function that returns random anything cannot be pure, because its result depends on something other than its arguments.

And that is a big boo-boo for pure FP, like Haskell. However, to be useful, even pure FP languages need to be able to deal with side-effects. Luckily, we have monads to help us with that; and in particular, the IO monad.

Just before I continue, I must point out that when I say "the _x_ monad", I mean that there is an instance of the ``Monad`` typeclass for the given type _x_. IO is a data structure with a function that performs some operation on the world and returns a new world and the result of the opreation. In Haskell-speak, it is:

```haskell
data IO a = IO (Realworld -> (Realworld, a))
```

As you can see, this is just a data type; so when I say the IO monad, I mean that there is 

```haskell
class Monad a where
  return :: a -> m a
  (>>=)  :: forall a b. m a -> (a -> m b) -> m b
```

```haskell
instance Monad IO where
  return    = returnIO
  (>>=)     = bindIO
  ...
```

With that in mind, let's create a person record, and then a way to generate said person.

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
    age       <- randomInt 101
    return $ Person firstName lastName age 
    where
      randomString = do
        len <- randomInt 10
        return $ take len (randomRs ('a', 'z') gen)
      randomInt max = (randomIO :: IO Int) >>= return . (`mod` max)
```

Just what is ``return . (`mod` max)``? It is the familiar ``f . g``, which you can read as _f_ following _g_; in other words, a function that takes what _g_ takes and returns what _f_ applied to what _g_ returns. Let's get back to decyphering the code. ``return`` is the function from the ``Monad`` typeclass, it takes some value that it packs into the monad (``IO`` here) and returns the ``IO`` with the given value. The bind (``>>=``) function binds ``m a`` over function ``a -> m b``, and returns ``m b``. This feels like it could fit, particularly for a special case where ``a == b``.

```haskell
return ::                     a -> m a
(>>=)  :: forall a b. m a -> (a -> m b) -> m b
```

We are supplying the first parameter (``IO Int``) in the application of ``>>=``, we just need the second one; of type ``Int -> IO Int``. The modulo function (``mod``) takes two numbers, ``(Num a) => a -> a -> a``. We can have ``return`` _following_ ``mod c`` to give us indeed a function from ``Int -> IO Int``. Therefore, in our code we have ``return . (`mod` max)``.

--- 

Once we are able to generate one random person, we can surely _replicate_ the generating process over a number of random persons.

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

Notice again the function composition ``.`` in the equation ``(> 50) . age``. The type of ``(> 50)`` is (nearly) ``Int -> Bool``; the ``Int`` here should represent the person's age. We want to say take ``age``, and apply whatever that returns to ``(> 50)``. In Scala syntax, this would be

```scala
def age(p: Person): Int = ...
def over50(p: Person) = (age _ andThen (50 >))(p)
val over50p = age _ andThen (50 >)
```

This is a bit clunky, and so we don't usually compose functions like this in Scala. Moreover, Scala is OO, so there is a method ``age`` on the ``Person`` type; similarly, Scala's ``List[A]`` contains the ``partition`` method takes the ``A => Boolean`` parameter. And so, we only write

```scala
val ps: List[Person] = ...
ps partition (_.age > 50)
```

However, in Haskell, the situation is completely different, and we make the most of the composition. Haskell code typically tries to be point-free, which roughly means that it tries to eliminate unnecessary values from both sides of Haskell equations.


```haskell
personOfAge :: Int -> Person
personOfAge age = Person "first" "last" age
```

The ``personOfAge`` here is pointwise, but we can eliminate the ``age`` element from both sides of the equation, giving us the point-free version:


```haskell
personOfAge :: Int -> Person
personOfAge = Person "first" "last"
```

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

To "see" the average, we "unwrap" the result of ``people >>= return . average``, whose type is ``IO Int``. Nota bene that ``people >>= return . average`` only returns ``IO Int``, it does not actually compute anything. To kick-off the computation, we must unwrap the value in the ``IO`` container, for example by requesting it to be displayed.
