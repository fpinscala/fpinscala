package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import Status._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = sys.error("placeholder")
}

trait Status {

}

object Status {

}

object Gen {

}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = sys.error("placeholder")
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = sys.error("placeholder")
}

trait SGen[+A] {

}

