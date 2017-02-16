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

  // Ex. 3.1: the value of `x` will be `3`. 

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


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(rest: List[A], i: Int): List[A] = rest match {
      case Nil => sys.error("empty list")
      case Cons(x, xs) =>
        if (i < n) go(xs, i + 1) else rest
    }
    go(l, 0)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = {
    val buffer = new scala.collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(rest: List[A]): List[A] = rest match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) =>
        List(buffer.toList: _*)
      case Cons(x, xs) =>
        buffer.append(x)
        go(xs)
    }
    go(l)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, accum) => accum + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumViaFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((accum, _) => accum + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((accum, a) => Cons(a, accum))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concat[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, List[A]())(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = l match{
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def mapViaFoldRight[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a, accum) => Cons(f(a), accum))

  def plusOne(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  def doublesToStrings(l: List[Double]): List[String] =
    map(l)(_.toString)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, List[A]())((a, accum) => if (f(a)) Cons(a, accum) else accum)

  def removeOdds(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addPairwise(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, addPairwise(xs1, xs2))
  }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
  }

  def exists[A](l: List[A])(f: A => Boolean): Boolean = l match {
    case Nil => false
    case Cons(x, xs) => if (f(x)) true else exists(xs)(f)
  }

  // Not very efficient as it computes strictly
  // all possible combinations in advance (lazyness would help).
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def sequences[A](l: List[A]): List[List[A]] = {
      val buffer = new scala.collection.mutable.ListBuffer[List[A]]
      @annotation.tailrec
      def go(accum: List[A], rest: List[A]): List[List[A]] = {
        buffer += reverse(accum)
        rest match {
          case Nil => List(buffer.toList: _*)
          case Cons(x, xs) => go(Cons(x, accum), xs)
        }
      }
      l match {
        case Nil => Nil
        case Cons(x, xs) => append(go(List(x), xs), sequences(xs))
      }
    }
    exists(sequences(sup))(_ == sub)
  }

}
