package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import language.higherKinds
import scala.annotation.tailrec

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  //  EXERCISE 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = { x => a1(a2(x)) }

    override def zero: A => A = (a: A) => a
  }


  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A = as match {
    case Nil => m.zero
    case h :: t => m.op(h, concatenate(t, m))
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)

  //  EXERCISE 10.6
  def foldMap_10_6[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    foldRight(as)(m.zero)((x, accum) => m.op(f(x), accum))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case h :: t => f(h, foldRight(t)(z)(f))
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(partialResult: B)(theList: List[A]): B = theList match {
      case Nil => partialResult
      case h :: t => go(f(partialResult, h))(t)
    }
    go(z)(as)
  }


  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    def go(left: Int, right: Int): B = {
      if (left < right) {
        val middle = (left + right) / 2
        m.op(go(left, middle), go(middle, right))
      }
      else f(as(left))
    }
    if (as.isEmpty) m.zero
    else go(0, as.length)
  }

  //  EXERCISE 10.9
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // this isn't a monoid
    val booleanOrder: Monoid[(Boolean, Int)] = new Monoid[(Boolean, Int)] {
      override def op(a1: (Boolean, Int), a2: (Boolean, Int)): (Boolean, Int) =
        (a1._1 && a2._1 && a1._2 < a2._2, a1._2 + a2._2)

      override def zero: (Boolean, Int) = (true, 0)
    }

    foldMapV(ints, booleanOrder)(a => (true, a))._1
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = a1 match {
      case Stub(chars1) => a2 match {
        case Stub(chars2) => Part("", true.compareTo(!chars1.isEmpty) + true.compareTo(!chars2.isEmpty), "")
        case Part(lStub, words, rStub) =>Part(lStub, true.compareTo(!chars1.isEmpty) + words, rStub)
      }
      case Part(lStub1, words1, rStub1) => a2 match {
        case Stub(chars) =>Part(lStub1, true.compareTo(!chars.isEmpty) + words1, rStub1)
        case Part(lStub2, words2, rStub2) => {
          // do product cartesia an see the cases
          val count1 = if (rStub2.isEmpty && lStub2.isEmpty) 0 else 1
          val count2 = if (rStub2.isEmpty && lStub2.isEmpty) 0 else 1
          Part("", words1 + words2 + count1 + count2, "")
        }
      }
    }

    override def zero: WC = Stub("")
  }

  def count(s: String): Int = ???

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B =
        (a: A) => B.op(a1.apply(a), a2.apply(a))

      override def zero: A => B = A => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    as.foldLeft(Map[A, Int]()) {
      (acc, k) => acc.updated(k, acc.getOrElse(k, 0) + 1)
    }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(Nil:List[A])((b, a) => a :: b)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case ::(head, tl) => f(head, foldRight(tl)(z)(f))
  }

  def foldRight2[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  @tailrec
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case ::(head, tl) => foldLeft(tl)(f(z, head))(f)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((accum, x) => mb.op(f(x), accum))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((accum, x) => mb.op(f(x), accum))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(a, b) => mb.op(foldMap(a)(f)(mb), foldMap(b)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
}

