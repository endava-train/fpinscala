package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => if (n == 0) l else drop(xs, n - 1)
    }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => if (xs == Nil) xs else Cons(x, init(xs))
    }

  //  EXERCISE 3.9 -> Compute the length of a list using foldRight.
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x,y) => 1 + y)

  //  EXERCISE 3.10
  //  Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
  //  for large lists (we say it’s not stack-safe). Convince yourself that this is the
  //  case, and then write another general list-recursion function, foldLeft, that is
  //  tail-recursive, using the techniques we discussed in the previous chapter. Here is its
  //  signature:

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f (z, x))(f)
    }
  }

  //  EXERCISE 3.11
  //  Write sum, product, and a function to compute the length of a list using foldLeft.

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Int]) =
    foldLeft(ns, 1)(_ * _)

  //  EXERCISE 3.12
  //  Write a function that returns the reverse of a list (given List(1,2,3) it returns
  //    List(3,2,1)). See if you can write it using a fold.

  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, Nil:List[A])((x, y) => Cons(y, x))

  //  EXERCISE 3.13
  //  Hard: Can you write foldLeft in terms of foldRight? How about the other way
  //  around? Implementing foldRight via foldLeft is useful because it lets us implement
  //  foldRight tail-recursively, which means it works even for large lists without overflow-
  //  ing the stack.

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldLeft(reverse(as), z)(f)

  //  EXERCISE 3.14
  //  Implement append in terms of either foldLeft or foldRight.

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  //  EXERCISE 3.15
  //  Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  //  should be linear in the total length of all lists. Try to use functions we have already
  //  defined.
  // -> I believe this is not linear of all lists
  def concatedList[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil:List[A])(append2(_, _))

  //  EXERCISE 3.16
  //  Write a function that transforms a list of integers by adding 1 to each element.
  //  (Reminder: this should be a pure function that returns a new List!)
  def addingOne(as: List[Int]): List[Int] = {
    as match {
      case Nil => Nil
      case Cons(x, y) => Cons(x + 1, addingOne(y)).copy()
    }
  }

  //  EXERCISE 3.17
  //  Write a function that turns each value in a List[Double] into a String. You can use
  //  the expression d.toString to convert some d: Double to a String.
  def doubleToString(as: List[Double]): List[String] = {
    foldRight(as, Nil:List[String])((x, y) => Cons(x.toString, y).copy())
  }

  //  EXERCISE 3.18
  //  Write a function map that generalizes modifying each element in a list while maintain-
  //  ing the structure of the list. Here is its signature:
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((x, y) => Cons(f(x), y).copy())

  //  EXERCISE 3.19
  //  Write a function filter that removes elements from a list unless they satisfy a given
  //  predicate. Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x, y) => if(f(x)) Cons(x, y) else y )

  //  EXERCISE 3.20
  //  Write a function flatMap that works like map except that the function given will return
  //  a list instead of a single result, and that list should be inserted into the final resulting
  //  list. Here is its signature:
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((x, y) => append2(f(x), y))

  //  EXERCISE 3.21
  //  Use flatMap to implement filter .
  //  println(flatMap(List(1, 2, 3, 46))(i => if (i % 2 == 0) List(i) else List()))

  //  EXERCISE 3.22
  //  Write a function that accepts two lists and constructs a new list by adding correspond-
  //  ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def joinSum(a1: List[Int], a2: List[Int]): List[Int] = {
    a1 match {
      case Nil => Nil
      case Cons(a, b) => a2 match {
        case Cons(c, d) => Cons(a + c, joinSum(b, d))
      }
    }
  }

  //  EXERCISE 3.23
  //  Generalize the function you just wrote so that it’s not specific to integers or addition.
  //  Name your generalized function zipWith.

  def zipWith[A, B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = {
    a1 match {
      case Nil => Nil
      case Cons(a, b) => a2 match {
        case Cons(c, d) => Cons(f(a, c), zipWith(b, d)(f))
      }
    }
  }

  //  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean


  def main(args: Array[String]): Unit = {

    println(zipWith(List(1,2,3), List(4,5,6))(_ + _))

    println(flatMap(List(1,2,3))(i => List(i,i)))

  }
}
