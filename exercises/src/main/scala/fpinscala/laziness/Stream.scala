package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    val buffer = new scala.collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(rest: Stream[A]): List[A] = rest match {
      case Cons(h, t) => buffer += h(); go(t())
      case _ => buffer.toList
    }
    go(this)
  }

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

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => if (n > 0) Some((h(), t().takeViaUnfold(n - 1))) else None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => if (p(h())) Some(h(), t().takeWhileViaUnfold(p)) else None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[AA >: A](s2: => Stream[AA]): Stream[AA] =
    foldRight(s2)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty[A], t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty[B]))
      case _ => None
    }
 
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(h, t) => Some(s, t())
      case _ => None
    }.append(Stream(empty))

  
   def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
    
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

  val fibs: Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] =
      cons(x, go(y, x + y))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).foldRight(empty[A]) { case ((a, s), _) =>
      cons(a, unfold(s)(f))
    }

  def fibsViaUnfold: Stream[Int] =
    unfold(0, 1) { case (f, s) => Some((s, (s, f + s))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))
}
