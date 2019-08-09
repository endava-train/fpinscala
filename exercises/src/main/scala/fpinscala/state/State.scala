package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //  EXERCISE 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (Math.abs(n + 1), nextRNG)
  }

  def nextNegative(rng: RNG): (Boolean, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (n < 0, nextRNG)
  }

  //  EXERCISE 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    lazy val maxValue = Math.abs(1.0 / Int.MinValue)
    (maxValue * n, nextRNG)
  }

  //  EXERCISE 6.3
  def double2(rng: RNG): Rand[Double] = {
    lazy val maxValue = Math.abs(1.0 / Int.MinValue)
    map(nonNegativeInt)(maxValue * _)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (theInt, nextIntRNG) = rng.nextInt
    val (theDouble, nextDoubleRNG) = double(rng)
    ((theInt, theDouble), nextIntRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((theInt, theDouble), nextIntRNG) = intDouble(rng: RNG)
    ((theDouble, theInt), nextIntRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n1, next1) = nonNegativeInt(rng)
    val (n2, next2) = nonNegativeInt(next1)
    val (n3, next3) = nonNegativeInt(next2)
    ((n1, n2, n3), next3)
  }

  //  EXERCISE 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val str = Stream.iterate(rng.nextInt)(x => x._2.nextInt).take(count)
    val nextRNG = str.map(_._2).last
    val theList = str.map(_._1).toList
    (theList, nextRNG)
  }

  //  EXERCISE 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  //  EXERCISE 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((x, accum) => map2(x, accum)((a, b) => a :: b))

  //  EXERCISE 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  //  EXERCISE 6.9
  def map69[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { it => unit(f(it)) }

  def map2_69[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => map(rb) { b => f(a, b) } }
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap { it => State.unit(f(it)) }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a => sb.map { b => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State{ (s: S) =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}
