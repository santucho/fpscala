package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(h,t) => go(t(), h()::acc)
      }
    }
    go(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_,t) if n > 0 => t().drop(n-1)
      case _ => this
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h,t) => p(h) && t().forAll(p)
  }

  def forAll_1(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,_) => Some(h())
  }

  def headOption_1: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a,sb) => cons(f(a),sb))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a,sb) =>
      if (f(a)) cons(a,sb)
      else sb
    )

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((b,sb) => cons(b,sb) )

  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a,sb) =>
      f(a) append sb
    )

  // Use unfold to implement map , take , takeWhile , zipWith (as in chapter 3), and zipAll.
  // The zipAll function should continue the traversal as long as either stream
  // has more elements—it uses Option to indicate whether each stream has been exhausted.

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (_, 0) => None
      case (Cons(h,_), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), x) => Some((h(), (t(), x-1)))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold(this,s2) {
      case (Empty, Empty) => None
      case (Cons(ha,ta), Empty) => Some((f(Some(ha()), None), (ta(), empty)))
      case (Empty, Cons(hb,tb)) => Some((f(None, Some(hb())), (empty, tb())))
      case (Cons(ha,ta), Cons(hb,tb)) => Some((f(Some(ha()), Some(hb())), (ta(), tb())))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

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

  def constant[A](a: A): Stream[A] = {
    lazy val constant: Stream[A] = Cons(() => a, () => constant)
    constant
  }

  def from(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Cons(() => n, () => from(n+1))
    tail
  }

  def from_1(n: Int): Stream[Int] =
    cons(n, from(n+1))

  val fibs: Stream[Int] = {
    def go(last: Int, actual: Int): Stream[Int]=
      cons(last, cons(actual, go(actual, last + actual)))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], z:S): Stream[A] = f(z) match {
      case None => this()
      case Some((a,z)) => go(cons(a,s),z)
    }
    go(Empty, z)
  }

  def unfold_1[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  //fibs, from, constant and ones from unfold
  val fibsViaUnfold: Stream[Int] =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) { n => Some(n,n+1) }

  def constant(n: Int): Stream[Int] =
    unfold(n) { n => Some(n,n)}

  //Ones is like constant but with 1 instead of n
}