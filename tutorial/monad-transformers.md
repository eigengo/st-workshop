#Monad transformers
Let's expand our random ``Person`` generator to include keep track of the count of people generated and to write out some values during the generating process. For motivation, let's say we want to see not just 

```haskell
[Person {firstName = "oiydgg", lastName = "dggzqucug", age = 90},Person {firstName = "cugc", lastName = "crxt", age = 97},Person {firstName = "", lastName = "gjvm", age = 47},Person {firstName = "nxwd", lastName = "d", age = 77},Person {firstName = "abpdwqyj", lastName = "dwqyj", age = 46},Person {firstName = "gwb", lastName = "bitdxx", age = 59},Person {firstName = "sfqhs", lastName = "hspy", age = 47},Person {firstName = "vzj", lastName = "poagqzwpx", age = 34},Person {firstName = "wpxvwmcb", lastName = "vwmcbrc", age = 19},Person {firstName = "c", lastName = "btyxarkl", age = 75}]
```

But also "messages" we recorded in each step, and the total number of steps:

```haskell
(([Person {firstName = "oiydgg", lastName = "dggzqucug", age = 90},Person {firstName = "cugc", lastName = "crxt", age = 97},Person {firstName = "", lastName = "gjvm", age = 47},Person {firstName = "nxwd", lastName = "d", age = 77},Person {firstName = "abpdwqyj", lastName = "dwqyj", age = 46},Person {firstName = "gwb", lastName = "bitdxx", age = 59},Person {firstName = "sfqhs", lastName = "hspy", age = 47},Person {firstName = "vzj", lastName = "poagqzwpx", age = 34},Person {firstName = "wpxvwmcb", lastName = "vwmcbrc", age = 19},Person {firstName = "c", lastName = "btyxarkl", age = 75}],
["P #0","P #1","P #2","P #3","P #4","P #5","P #6","P #7","P #8","P #9"]),
10)
```

We could, of course, include the required values in the return types of the ``person`` and ``people`` functions, adding the required inner functions to keep track of the state. Unfortunately, this would quickly get out of hand; especially if you ever wanted to add yet another little piece of information.

Fortunately, we can get our hands on the IO monad that the ``person`` function returns, and we can introduce more functionality by transforming it into another monad; a monad that can carries the additional information we need. When evaluating the final result, we will peel away the transformed monads, pulling out the extra information we record in them along the way.

##Monad transformers to the rescue
Let's write a function that transforms the ``IO`` monad around ``IO Person``, and adds the "debug" messages and the running count of generated persons. First, I'll define a type that will name the _generating_ process, allowing us to specify the type of the messages (the type we will be writing), and the type of the "final" output value.

```haskell
  type Generate w a = WriterT w (StateT Integer IO) a
```

Here, I say that the type ``Generate`` carries, in addition to the type ``a``, some writable field of type ``w``, and it keeps track of some ``Integer`` state. (This fits our goal where we want to keep track of some tracing messages, and we want to keep the running count in the generating process.) We can, of course eliminate the final ``a`` and get

```haskell
  type Generate w = WriterT w (StateT Integer IO)
```

Great. Now we can define a function that given ``IO a`` returns ``Generate [String] a``.

```haskell
  personT :: IO a -> Generate [String] a
  personT person = do
    count <- get
    tell  ["P #" ++ show count]
    put   (count + 1)
    ???
```

The only complication is in the last line. We cannot return ``IO a``, we need to return some other ``m a`` that can be transformed into ``Generate [String] a``. To do so, we will _lift_ the value ``IO a`` by applying ``liftIO person``, giving us the final body of ``personT``.

```haskell
  personT :: IO a -> Generate [String] a
  personT person = do
    count <- get
    tell  ["P #" ++ show count]
    put   (count + 1)
    liftIO person
```

Now, to generate (say) 1000 ``Person``s, but keeping track of the messages and count in each step, we can use the same ``replicateM`` we used in the plain monadic style earlier. Of course! We have not created any new structure, we are still dealing with monads. 

```haskell
  peopleT :: (Monoid w) => Generate w a -> Generate w [a]
  peopleT = replicateM 1000
```

##Unwrapping
Suppose you now have a value ``Generate [String] [Person]`` assigned to some variable. (In GHCi, you can evaluate ``let gen = peopleT $ personT person``.) To actually see the result, you need to unwrap the values you have written into the state and writer. You can do that by applying the ``Generate w a`` value to ``runStateT (runWriterT gen) s0``, where ``gen`` is ``Generate w a`` and ``s0`` is some initial value for the state (``Integer`` in our case).

```haskell
  runGenerate :: Generate w a -> IO ((a, w), Integer)
  runGenerate gen = runStateT (runWriterT gen) 0
```

To see it all in action, evaluate

```haskell
> runGenerate (peopleT $ personT person)
(([Person {firstName = "rftdlfwrm", lastName = "d", age = 55},Person {firstName = "dbey", lastName = "ybrk", age = 25},Person {firstName = "iq", lastName = "po", age = 51},Person {firstName = "xogdi", lastName = "di", age = 50},Person {firstName = "", lastName = "khqxtrn", age = 30},Person {firstName = "nnophkgd", lastName = "ph", age = 64},Person {firstName = "", lastName = "tck", age = 22},Person {firstName = "mnbjmzrh", lastName = "jmzrhvlk", age = 84},Person {firstName = "lky", lastName = "ud", age = 95},Person {firstName = "kdm", lastName = "xkzggx", age = 72}],["P #0","P #1","P #2","P #3","P #4","P #5","P #6","P #7","P #8","P #9"]),10)
```