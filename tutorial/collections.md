#Collections
Programs rarely deal with just numbers on their own. Numbers, and other things, like to appear in collections. Scala's functional nature, object-orientation, and its type inference is an excellent fit for dealing with collections of things.

```scala
val l = List(1, 2, 3, 4, 5, 6)
```

What happened here? Scala calls [static] function ``def List.apply[A](as: A*): List[A]``. Think of it as factory method for lists. (``A*`` is Scala-speak for Java's ``A...``.)

Let's now do some quick list gymnastics:

* Square every element in the list: ``l.map(x => x * x)``,
* Filter to limit: ``l.filter(x => x < 4)``,
* Partition: ``l.partition(x => x < 4)``

---

To progress further, we need slightly more numbers. We can use Scala's ranges.

```scala
val l = 1 to 1000000
```

WHAA? This is Scala's sugar for ``1.to(1000000)``. Recall that Scala is OO, so ``1`` is an instance of ``Int``; and we can call methods on it. (Actually, this case is ever-so-slightly more hairy, but let's not worry about that now.)

```scala
l partition (x => x < 40000)
```

Let's keep partitioning; to do so nicely, we will need to have a long list of random numbers.

```scala
import scala.util.Random
// val r = List.fill(100000)(Random.nextInt(100))
```

Let's imagine that these numbers represent peoples' ages. You know, let's not imagine, but define the ``Person`` types instead.

```scala
case class Person(firstName: String, lastName: String, age: Int)
```

And instead of generating the random numbers, we'll generate random people.

```
val people = List.fill(100000) {
  def randomName = Random.alphanumeric.take(Random.nextInt(10)).mkString
  def randomAge  = Random.nextInt(100)
  Person("F" + randomName, "L" + randomName, randomAge)
}
```

First question. What's the sum of all the ages?

```scala
people.foldLeft(0) { (sum, p) => sum + p.age }
```

Or, perhaps more succinctly

```scala
people.foldLeft(0)(_ + _.age)
```

Next, what's the average age?

Now, we can easily partition our population into _optimistic and young_ and _cynical and decrepit_. Obviously, we all fall into the first group, so let's partition accordingly:

```
val (decrepit, young) = people partition (x => x.age > 49)
val (decrepit, young) = people partition (_.age > 49)
```

---

We can then be asking questions like, "how many different people are there in every age group?"

```scala
val ages = decrepit groupBy (x => x.age)
val ages = decrepit groupBy (_.age)
```

And let's say we don't want to see the people (it's obvious that the group with key ``0`` will contain the ``Person`` instances with ``age == 0``); instead, let's say we're interested in how many values there are in every group.

```scala
ages map { case (k, v) => (k, v.size) }
ages mapValues (v => v.size)
ages mapValues (_.size)
```

---

Let's close with Fibbonaci's numbers. How would you do it normally? We are going to use a lazy collection which is like a list but it only gets materialized when it is used. So we can define an _infinite list_.

```scala
val fibs: Stream[BigInt] = 0 #:: fibs.scanLeft(BigInt(1))(_ + _)
```

And then ask Scala to evaluate the first 10:

```scala
fibs.take(10).toList
```
