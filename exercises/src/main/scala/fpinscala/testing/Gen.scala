package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Simple

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def &&(p: Prop): Prop = new Prop {
    override def check = p.check
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen{
  def apply[A](value: State[RNG, A]): Gen[A] = ???

  def unit[A](a: => A): Gen[A] = Gen(State((_, a)))
}

case class Gen[A](sample: State[RNG,A]) {

  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] = Gen(State(RNG.nextNegative))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap((value: Boolean) => if (value) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

  def unsized: SGen[A] = ???
}

trait SGen[+A] {

}

