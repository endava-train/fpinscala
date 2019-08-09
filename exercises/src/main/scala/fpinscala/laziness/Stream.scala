package fpinscala.laziness

import fpinscala.laziness.Stream.unfold

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((x, accum) => if (p(x)) Cons(() => x, () => accum) else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((x, accum) => Cons(() => f(x), () => accum))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((x, accum) => if (f(x)) Cons(() => x,() => accum) else accum)

  def append[B >: A](e: => B): Stream[B] =
    foldRight[Stream[B]](Cons(() => e, () => Empty))((x, accum) => Cons(() => x, () => accum))

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  //  EXERCISE 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipWith(s).foldRight(true)((x, accum) => x._1.equals(x._2) && accum)

  // EXERCISE 5.15
  def tails: Stream[Stream[A]] = {
      def go(): Stream[Stream[A]] = unfold(this) {
        case Empty => None
        case Cons(h, t) => Some(Cons(h, t), t())
      }
      go().append(Stream())
  }

  //  EXERCISE 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    this.tails.map13(_.foldRight(z)(f))
  }

  // EXERCISE 5.13
  def map13[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def take13(n: Int): Stream[A] = unfold((n, this)) {
    case (a, str) => if (a == 0) None else str match {
      case Empty => None
      case Cons(h, t) => Some(h(), (a - 1, t()))
    }
  }

  def takeWhile13(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
  }

  /*
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
  applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
  */

  def zipWith[B](s2: Stream[B]): Stream[(A, B)] = unfold((this, s2)) {
    case (Empty, _) | (_, Empty) => None
    case (Cons(h1, t1) , Cons(h2, t2) ) => Some((h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =  unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1) , Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int]  =
      cons(a, go(b, a + b))
    go(0, 1)
  }

  def facto(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int]  =
      cons(a * b, go(a * b, b + 1))
    go(1, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def ones2(): Stream[Int] = constant2(1)

  def constant2[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))

  def from2(n: Int): Stream[Int] = unfold(1)(x => Some((x, x + 1)))

  def fibs2(): Stream[Int] = unfold((0, 1)) {
    case (a, b) => Some((a, (b, a + b)))
  }

  def facto2(): Stream[Int] = unfold((1, 1)) {
    case (a, b) => Some((a * b, (a * b, b + 1)))
  }


  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).take13(2).map13(_ + 2).toList)
    println(Stream(1, 2, 3).drop(1).toList)
    println(Stream(4, 2, 3).takeWhile2(_ == 4).toList)
    println(facto2().takeWhile13(_ < 1000000).toList)
    println(facto2().take13(10).zipWith(fibs().take(10)).toList)
    println(facto2().takeWhile13(_ < 100).zipAll(fibs().take(10)).toList)
    println(Stream(4, 2, 3).startsWith(Stream()))
    println(Stream(4, 2, 3).tails.map13(_.toList).toList)
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }
}