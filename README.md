#1UP Scala workshop at Manchester university
Jan Machacek (<janm@cakesolutions.net>, @honzam399) and Nigel Warren (<nigel.warren@underscoreconsulting.com>, @NigeWarren) will show how you can make the most of Scala's functional, strongly-typed and object-oriented nature to solve problems that would otherwise span over many, many lines of Java code.

###Setup on proper OSs
Come to the talk ready with your computer running sbt; we recommend using [sbt-extras](https://github.com/paulp/sbt-extras). Download the shell script and check that you can run ``sbt``.

```bash
~/sandbox/oneup-nov13$ sbt
~/sandbox/oneup-nov13 doesn't appear to be an sbt project.
If you want to start sbt anyway, run:
  /usr/bin/sbt -sbt-create
~/sandbox/oneup-nov13$
```

To give you a starting point for _proper_ Scala project, we will need a build file. It can be embarassingly simple: empty.

```bash
~/sandbox/oneup-nov13$ touch build.sbt
```

We will be using postfix operators, so you may want to enable them in the ``build.sbt``:

```scala
initialCommands in console := "import language.postfixOps"
```

If all worked as expected, you should be able to run ``sbt console``:

```bash
~/sandbox/oneup-nov13$ sbt console
[info] Set current project to oneup-nov13 (in build file:~/sandbox/oneup-nov13/)
[info] Starting scala interpreter...
[info]
import language.postfixOps
Welcome to Scala version 2.10.2 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_40).
Type in expressions to have them evaluated.
Type :help for more information.

scala>
```

You are now seeing Scala's REPL, where we will be typing some Scala pixie-dust.

###Setup on Windows
Windows setup is rather clunky, especially if you're not using Cygwin. Follow [http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) for instructions. You need to get to the same position as your Linux / UNIX friends: running ``sbt`` on the command prompt works; running ``sbt console`` in a directory with a valid ``build.sbt`` file shows the Scala REPL prompt.
