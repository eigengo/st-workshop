#Typeclasses

Typeclass is a concept in Scala and Haskell (and in some other strongly-typed languages). The best way
to think about a typeclass is that it is an interface, which defines functions. So, having a typeclass

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
```

is the equivalent of a ``trait`` in Scala parametrized with type variable:

```scala
trait Monoid[A] {
  def mempty: A
  def mappend(a1: A, a2: A): A
}
```

If you look at these typeclass definitions, you should see that these are just the interfaces. They 
contain no behaviour. Let's now find out how to define instances of typeclasses; a typeclass instance is
an implementation of the interface (say, the ``Monoid`` above) for a specific type. Consider:

```haskell
instance Monid Int where
  mempty = 0
  mappend a b = a + b
```

Of course, being Haskell, we can eliminate the values in ``mappend`` and write just ``mappend = (+)``. Now, in Scala, we can have:

```scala
class IntMonoid extends Monoid[Int] {
  def mempty = 0
  def mappend(a: Int, b: Int) = a + b
}
```

If we now define a polymorphic function (with respect to its parameters), we can require that there be an
instance of typeclass for the type.

```haskell
sigma :: (Monoid a) => Int -> Int -> (Int -> Int) -> (Int -> a) -> a
sigma a b inc comp =
  if a > b then mempty else mappend (comp a) (sigma (inc a) b inc comp)
```

Before I dig into the details, let me show equivalent code in Scala:

```scala
def sigma[A](a: Int, b: Int, inc: Int => Int, comp: Int => a)(implicit m: Monoid[A]): A =
  if (a > b) m.mempty else m.append(comp(a), sigma(inc(a), b, inc, comp))
```

So, in Scala, we have to be a bit more explicit; we need to tell the compiler which monoid instance 
(instance here is the OO instance, a value that implements ``Monoid[A]``). However, the compiler can
search the _implicit scope_ and find an instance of ``Monoid[Int]``; of course reporting errors if it 
cannot find exactly one instance.

Now, ``class IntMonoid extends Monoid[Int]`` is not an instance. It is a class definition, and therefore 
the compiler will complain that there is no implicit instance of ``Monoid[Int]``. In Scala, we have to 
provide an implicit value. We can do so by writing:

```scala
implicit val intMonoid = new IntMonoid
```

Or, in this particular case, we can actually make the ``Monoid[Int]`` a singleton! 

```scala
implicit object IntMonoid extends Monoid[Int] {
  def mempty = 0
  def mappend(a: Int, b: Int) = a + b
}
```

If this ``object IntMonoid`` is in the current scope, the Scala compiler will be able to supply it as 
the value of the ``implicit m: Monoid[A]`` parameter; and our function compiles & runs as expected.

##Haskell again
Compare the code in Haskell:

```haskell
sigma :: (Monoid a) => Int -> Int -> (Int -> Int) -> (Int -> a) -> a
sigma a b inc comp = ...
```

And the Scala code

```scala
// option 1: explicit implicits
def sigma[A](a: Int, b: Int, inc: Int => Int, comp: Int => A)(implicit m: Monoid[A]): A =
  if (a > b) m.mempty else m.append(...)
// option 2: type bounds
def sigma[A : Monoid](a: Int, b: Int, inc: Int => Int, comp: Int => A): A =
  if (a > b) implicitly[Monoid[A]].mempty else implicitly[Monoid[A]].append(...)
// consume implicit
def sigma[A : Monoid](a: Int, b: Int, inc: Int => Int, comp: Int => A): A = {
  val m = implicitly[Monoid[A]]
  if (a > b) m.mempty else m.mappend(...)
}
```

Because Scala is OO, there are more ways in which you provide the typeclass instances; even though
ultimately, you have to have an implicit value available in the implicit scope. You can, however
write

```scala
// implicit singleton value
implicit object IntMonoid extends Monoid[Int] { ... }

// implicit anonymous implementation
implicit val intMonoid = new Monoid[Int] { ... }

// implicit function that returns a new Monoid[Int] every time
implicit def intMonoid = new Monid[Int] { ... }

// implicit named implementation
class IntMonoid extends Monoid[Int] { ... }
implicit val intMonoid = new IntMonoid
```

One might now think that in Haskell, the typeclass instances are equivalent of ``implicit object`` construct in Scala. Well...!

##Typeclass "inheritance"
We can have a kind of inheritance in our typeclasses. Let's add typeclass for ``Group``.

```haskell
class Group a where
  mempty :: a
  mappend :: a -> a -> a
  inverse :: a -> a
```

This feels like too much typing. We would really like to "inherit" all functions from ``Monoid a`` and
only provide the ``inverse``. Inheritance to the rescue:

```haskell
class (Monoid a) => Group a where
  inverse :: a -> a
```

Here, we have class ``Group`` for some type ``a``, that "inherits" functions from ``Monoid`` for the 
same a. To create instance of ``Group Int``, we can now only provide the one remaining function.

```haskell
instance Group Int where
  inverse = ((-1) *)
```

And we can now happily require these instances:

```haskell
example :: (Group a) => a -> a
example a = inverse (mappend a a)
```

---

We can do the same thing in Scala, of course. Let's define ``Group[A]``, which extends ``Monoid[A]``.

```scala
trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}
```

Now, how do we go about creating instances of ``Group[A]`` if we already have ``Monoid[A]``? We could, 
of course write the whole thing:

```scala
implicit object IntGroup extends Group[Int] {
  def mempty = 0
  def mappend(a: Int, b: Int) = a + b
  def inverse(a: Int) = -a
}
```

Or (inefficiently!) we can write code that's similar to the Haskell code:

```scala
implicit def intGroup(implicit m: Monoid[Int]): Group[Int] = new Group[Int] {
  def mempty = m.empty
  def mappend = m.append
  def invserse(a: Int) = -a
}
```

Here you can see that the compiler will create an instance of ``Group[Int]``, as long as it already
has an instance of ``Monoid[Int]``. Unfortunately, this is where Haskell's typeclasses are indeed
first-class citizens really shine; Scala's approach is not as efficient or elegant.

#What are these typeclasses?
A good way of thinking about the typeclasses is that they represent "strategies" based on types.
Our ``Monoid Int`` provided strategy to create empty ``Int``s and to append two ``Int``s together.
In plain old Java, equivalent code would be:

```java
interface Function<A, B> {
  B apply(A a);
}

interface Monoid<A> {
  A mempty();
  A mappend(A a, A b);
}

public static A <A>sigma(int a, int b, 
                         Function<Int, Int> inc,
                         Function<Int, A> comp,
                         Monoid<A> m) {
    if (a > b) return m.mempty();
    return m.mappend(comp.apply(a), sigma(inc.apply(a), b, inc, comp, m);
}
```

:(
