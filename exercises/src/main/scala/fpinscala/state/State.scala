package fpinscala.state

import fpinscala.state.Candy.update
import fpinscala.state.State.{get, modify, sequence}

import scala.annotation.tailrec

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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i,r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    ((i / (Int.MaxValue + 1)).toDouble, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d,r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d,r) = double(rng)
    val (i,r2) = r.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, l: List[Int]): (List[Int], RNG) = {
      if(count == 0)
        (l, rng)
      else {
        val (h, r) = rng.nextInt
        go(count - 1, r, h::l)
      }
    }
    go(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a,r1) = ra(rng)
      val (b,r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((e, acc) => map2(e,acc)(_::_))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a,r) = f(rng)
      val rb = g(a)
      rb(r)
    }
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(f andThen unit)
  }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}

import State._

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S, A] =
    State((s:S) => (a,s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @annotation.tailrec
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a,s1) = this.run(s)
      f(a).run(s1)
    })
}

/**
  * - Inserting a coin into a locked machine will cause it to unlock if there’s any
  * candy left.
  * - Turning the knob on an unlocked machine will cause it to dispense candy and
  * become locked.
  * - Turning the knob on a locked machine or inserting a coin into an unlocked
  * machine does nothing.
  * - A machine that’s out of candy ignores all inputs.
  *
  * The method simulateMachine should operate the machine based on the list of inputs
  * and return the number of coins and candies left in the machine at the end. For example,
  * if the input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully bought,
  * the output should be (14, 1).
  * def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]
  * */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs map (modify[Machine] _ compose update)) flatMap (_ =>
      get map (s => (s.coins, s.candies))
    )
}
