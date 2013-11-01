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

![Sum(a by 4, b)(1/(i*(i+2))](http://latex.codecogs.com/gif.latex?%5Csum_%7Bi%3Da%5C%2520by%5C%25204%7D%7D%5Eb%2520%5Cfrac%7B1%7D%7Bi(i%2B2)%7D)