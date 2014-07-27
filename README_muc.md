## Set Up

Suggested workflow: Everyone creates his personal repository, cloned from the main "fpinscala" project. Everyone pushes the exercises to her personal repository only. This way all repositories are visible in one place, and everyone can look into the commits made by others and comment. Common changes like additional scala tests can be pushed to the main repository and everyone can pull them into his personal repo.

Here are the steps to setup everything.

Create the personal repository, go to https://github.com/fpinscala-muc and create a new repository called "pfinscala-<YOUR-GITHUB-USERNAME>", select "public" and don't initialize the repo yet.

Clone the "main" respsitory:

    $ git clone git@github.com:fpinscala-muc/fpinscala.git
    $ cd fpinscala

If you use your work notebook you may not want to use your real name and TD email address for commits as they will be visible in public, in that case configure your username and email for the repository only:

    $ git config user.email "youremail@example.com"
    $ git config user.name "Your name or pseudonym"

Configure and initialize your personal repository:

    $ git remote remove origin
    $ git remote add origin git@github.com:fpinscala-muc/fpinscala-<YOUR-GITHUB-USERNAME>.git
    $ git remote add upstream git@github.com:fpinscala-muc/fpinscala.git
    $ git push -u origin master



## Chapter 1: What is functional programming?
### Additional links
* http://www.youtube.com/watch?v=1gZAqJA2pEk This video by Paul Chiusano is about an earlier version (with a different example) of the first chapter. It gives a nice intro & summary what the book is up to.
* http://www.youtube.com/watch?v=-FRm3VPhseI&index=2&list=PL693EFD059797C21E The Clean Code Talk with the credit card example (~ 19 min) where he had to use his own credit card # for testing and had the money indeed debited J I think this inspired the example in the book. For the other videos just search for “Clean Code Talk on youtube).
* http://steve-yegge.blogspot.de/2006/03/execution-in-kingdom-of-nouns.html The Kingdom of Nouns
* http://www.manning.com/suereth/Suereth_MEAP_CH01.pdf First chapter of “Scala in Depth”, discussing FP vs. OOP
* http://james-iry.blogspot.de/2007/08/kingdom-of-nerbs.html

## Chapter 2: Getting started with functional programming in Scala
### Additional links
