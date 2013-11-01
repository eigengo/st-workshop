#Introduction

Scala is an object-functional, strongly-typed language. Let's see how it all fits together. We begin by exploring its cool side by writing a function that sums all numbers from 0 to some _n_.

![Sum(a, b)(i)](http://latex.codecogs.com/gif.latex?%5Csum_%7Bi%3Da%7D%5Eb%2520i)

Of course, _without_ mutation, and _with_ recursion.


```scala
def sumInts(a: Int, b: Int): Int = if (a > b) 0 else a + sumInts(a + 1, b)
```

---

Let's modify this a bit and do a sum-of-squares.

![Sum(a, b)(i ^ 2)](http://latex.codecogs.com/gif.latex?%5Csum_%7Bi%3Da%7D%5Eb%2520i%5E2)

It's essentially the same thing:

```scala
def sumSquares(a: Int, b: Int): Int = if (a > b) 0 else (a * a) + sumSquares(a + 1, b)
```

---

These two functions only differ by the operation (``a`` or ``a * a``), otherwise they are the same thing. And so, we'd like to extract this same thing. That's just good engineering. Let's compute pi using Leibnitz's pi-sum.

![Sum(a by 4, b)(1/(i*(i+2))](http://latex.codecogs.com/gif.latex?%5Csum_%7Bi%3Da%5C%20by%5C%204%7D%7D%5Eb%20%5Cfrac%7B1%7D%7Bi%5E2%2B2i%7D)

This converges to

![pi / 8](http://latex.codecogs.com/gif.latex?%5Cfrac%7B%5Cpi%7D%7B8%7D)

```scala
def piSum(a: Int, b: Int): Double = if (a > b) 0 else (1.0 / (a * a + a * 2)) + piSum(a + 4, b)
```

---

Let's extract the pattern, which is _sum_. We are summing from lower bound to some upper bound, computing next value of the index, and performing some computation in every step:

```scala
def sum(a, b, inc, comp) = if (a > b) 0 else comp(a) + sum(inc(a), b, inc, comp)
```

Let's define the types properly:

```scala
def sum(a: Int, b: Int, inc: Int => Int, comp: Int => Int): Int =
  if (a > b) 0 else comp(a) + sum(inc(a), b, inc, comp)
```

And then use:

```scala
sum(0, 5, x => x + 1, x => x)
sum(0, 5, x => x + 1, identity)
sum(0, 5, (1).+, identity)
sum(0, 5, 1 +, identity)
```

Here, we take advantage of Scala's power. We see:

* Function literals: ``x => x + 1``,
* Local type inference: we don't need to write ``x: Int => x + 1``,
* Automatic lifting of method to functions. ``def identity[A](a: A): A = a`` is lifted to be ``a: A => a``; and the type ``A`` is inferred to be ``Int`` ,
* Object-functional nature: ``(1).+`` is the function called ``+`` in instance of type ``Int``, namely ``1``. It takes one more parameter of type ``Int`` and returns ``Int``. It can be lifted to match the parameter type ``Int => Int``.



 We see function literals (``x => x + 1``), we 