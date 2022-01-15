[![Join the chat at https://gitter.im/fpinscala/fpinscala](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/fpinscala/fpinscala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) 

This repository contains exercises, hints, and answers for the book
[Functional Programming in Scala](http://manning.com/bjarnason/). Along
with the book itself, it's the closest you'll get to having your own
private functional programming tutor without actually having one.

There are two main branches in this repository:
 - [first-edition](https://github.com/fpinscala/fpinscala/tree/first-edition)
 - [second-edition](https://github.com/fpinscala/fpinscala/tree/second-edition)

Be sure to select the branch which matches the edition of the book you are reading!

Here's how to use this repository:

Each chapter in the book develops a fully working library of functions
and data types, built up through a series of exercises and example code
given in the book text. The shell of this working library and exercise
stubs live in
`src/main/scala/fpinscala/exercises/<chapter-description>`, where
`<chapter-description>` is a package name that corresponds to the
chapter title (see below). When you begin working on a chapter, we
recommend you open the exercise file(s) for that chapter, and when you
encounter exercises, implement them in the exercises file and make sure
they work.

If you get stuck on an exercise, let's say exercise 4 in the chapter,
you can find hints in `answerkey/<chapter-description>/04.hint.txt` (if
no hints are available for a problem, the file will just have a single
'-' as its contents) and the answer along with an explanation of the
answer and any variations in
`answerkey/<chapter-description>/04.answer.scala` or
`04.answer.markdown`. The finished Scala modules, with all answers for
each chapter live in
`src/main/scala/fpinscala/answers/<chapter-description>`. Please feel
free to submit pull requests for alternate answers, improved hints, and
so on, so we can make this repo the very best resource for people
working through the book.

Chapter descriptions:

* Chapter 2: gettingstarted
* Chapter 3: datastructures
* Chapter 4: errorhandling
* Chapter 5: laziness
* Chapter 6: state
* Chapter 7: parallelism
* Chapter 8: testing
* Chapter 9: parsing
* Chapter 10: monoids
* Chapter 11: monads
* Chapter 12: applicative
* Chapter 13: iomonad
* Chapter 14: localeffects
* Chapter 15: streamingio

### Setup build environment

You'll need a Java development kit installed as well as the [SBT](http://scala-sbt.org) build tool. If you don't have these tools, we can get them via Couriser. First, install Coursier by choosing an installation method for your operating system on this page: https://get-coursier.io/docs/cli-installation. Then run `cs setup`. This will install Java, Scala, and the SBT build tool.

You'll also likely want an editor that's aware of Scala syntax. [VSCode](https://code.visualstudio.com) with the [Metals](https://scalameta.org/metals/docs/editors/vscode.html) extension works great.

### Building

To build the code for the first time, if on windows:

    $ sbt.cmd

If on mac/linux, first make sure you have not checked out the code onto
an encrypted file system, otherwise you will get compile errors
regarding too long file names (one solution is to put the fpinscala repo
on a unencrypted usb key, and symlink it into your preferred code
location).

    $ sbt

Once it is finished launching, you'll get a prompt from
which you can issue commands to build and interact with your code. Try
the following:

    > compile

This compiles all exercises and answers. You can also do:

    > console

to get a Scala REPL with access to exercises and answers, and

    > run

To get a menu of possible main methods to execute.

To run unit-tests for a file you can do:

    > testOnly fpinscala.exercises.gettingstarted.GettingStartedSuite

To run all unit-tests:

    > test

All code in this repository is
[MIT-licensed](http://opensource.org/licenses/mit-license.php). See the
LICENSE file for details.

Have fun, and good luck! Also be sure to check out [the community
wiki](https://github.com/fpinscala/fpinscala/wiki) for the **chapter
notes**, links to more reading, and more.

_Paul, Rúnar, and Michael_

