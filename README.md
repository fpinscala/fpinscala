This repository contains exercises, hints, and answers for the book [Functional Programming in Scala](http://manning.com/bjarnason/). Along with the book itself, it's the closest you'll get to having your own private functional programming tutor. 

Here's how to use this repository:

Each chapter in the book develops a fully working library of functions and data types, built up through a series of exercises and example code given in the book text. The shell of this working library and exercise stubs live in `exercises/src/main/scala/fpinscala/<chapter-description>`, where `<chapter-description>` is a package name that corresponds to the chapter title. When you begin working on a chapter, we recommend you open the exercise file(s) for that chapter, and when you encounter exercises, implement them in the exercises file and make sure they work.

If you get stuck on an exercise, let's say exercise 4 in the chapter, you can find hints in `answerkey/<chapter-description>/4.hint.txt` (if no hints are available for a problem, the file will just have a single '-' as its contents) and the answer along with an explanation of the answer and any variations in `answerkey/<chapter-description>/4.answer.txt`. The finished Scala modules, with all answers for each chapter live in `answers/src/main/scala/fpinscala/<chapter-description>`. 

To build the code for the first time, if on windows:

    $ .\sbt.cmd

If on mac/linux:

    $ chmod a+x ./sbt
    $ ./sbt

This will download and launch [sbt](https://github.com/harrah/xsbt/wiki/), a build tool for Scala. Once it is finished downloading, you'll get a prompt from which you can issue commands to build and interact with your code. Try the following: 

    > project exercises
    > compile

This switches to the exercises project, where your code lives, and compiles the code. You can also do:

    > console

to get a Scala REPL with access to your exercises, and

    > run

To get a menu of possible main methods to execute. 

All code in this repository is [MIT-licensed](http://opensource.org/licenses/mit-license.php). See the LICENSE file for details. 

Have fun, and good luck!

--Paul and Rúnar
