/**
  * Created by xpray on 2017/3/26.
  */

sealed trait Stream[+A] {
  def toList: List[A]
  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A]
  def takeWhile2(p: A => Boolean): Stream[A]
  def forAll(p: A => Boolean): Boolean
  def foldRight[B](z: => B)(f: (A, => B) => B): B
  def headOption: Option[A]
  def map[B](f: A => B): Stream[B]
  def filter(p: A => Boolean): Stream[A]
  def append[B >: A](s: Stream[B]): Stream[B]
  def flatMap[B](f: A => Stream[B]): Stream[B]
  def exists(p: A => Boolean): Boolean
  def scanRight[B >: A](z: B)(f: (A, B) => B): Stream[B]
}

case object EmptyStream extends Stream[Nothing] {

  override def toList: List[Nothing] = Nil

  override def take(n: Int): Stream[Nothing] = EmptyStream

  override def drop(n: Int): Stream[Nothing] = EmptyStream

  override def takeWhile(p: (Nothing) => Boolean): Stream[Nothing] = EmptyStream

  override def forAll(p: (Nothing) => Boolean): Boolean = true

  override def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z

  override def takeWhile2(p: (Nothing) => Boolean): Stream[Nothing] = EmptyStream

  override def headOption: Option[Nothing] = None

  override def map[B](f: (Nothing) => B): Stream[B] = EmptyStream

  override def filter(p: (Nothing) => Boolean): Stream[Nothing] = EmptyStream

  override def flatMap[B](f: (Nothing) => Stream[B]): Stream[B] = EmptyStream

  override def append[B >: Nothing](s: Stream[B]): Stream[B] = s

  override def exists(p: (Nothing) => Boolean): Boolean = false

  override def scanRight[B >: Nothing](z: B)(f: (Nothing, B) => B): Stream[B] = Stream(z)
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {

  override def toList: List[A] = h() :: t().toList

  override def take(n: Int): Stream[A] =
    if (n == 0) EmptyStream
    else Stream.cons(h(), t().take(n - 1))

  override def drop(n: Int): Stream[A] =
    if (n == 0) this
    else t().drop(n - 1)

  override def takeWhile(p: (A) => Boolean): Stream[A] =
    if (p(h())) Stream.cons(h(), t().takeWhile(p))
    else EmptyStream

  override def forAll(p: (A) => Boolean): Boolean =
    if (p(h())) t().forAll(p)
    else false

  override def foldRight[B](z: => B)(f: (A, => B) => B): B = f(h(), t().foldRight(z)(f))

  override def takeWhile2(p: (A) => Boolean): Stream[A] =
    foldRight(EmptyStream: Stream[A])((_, y) => if (p(h())) Stream.cons(h(), t().takeWhile2(p)) else y)

  override def headOption: Option[A] = Some(h())

  override def map[B](f: (A) => B): Stream[B] =
    foldRight(EmptyStream: Stream[B])((x, y) => Stream.cons(f(x), y))

  override def filter(p: (A) => Boolean): Stream[A] =
    foldRight(EmptyStream: Stream[A])((x, y) => if (p(x)) Stream.cons(x, y) else y )

  override def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((x, y) => Stream.cons(x, y))

  override def flatMap[B](f: (A) => Stream[B]): Stream[B] =
    foldRight(EmptyStream: Stream[B])((x, y) => f(x).append(y))

  override def exists(p: (A) => Boolean): Boolean = if (p(h())) true else t().exists(p)

  override def scanRight[B >: A](z: B)(f: (A, B) => B): Stream[B] = {
    val right = t().scanRight(z)(f)
    right match {
      case Cons(hr, _) => Stream.cons(f(h(), hr()), right)
      case _ => ???
    }
  }
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = EmptyStream

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(x: Int, y: Int): Stream[Int] = Stream.cons(x, fibs(y, x + y))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, nz)) => Stream.cons(a, unfold(nz)(f))
    }
  }

  def fibsByUnfold(x: Int, y: Int) = unfold((x, y))(s => Some(s._1, (s._2, s._1 + s._2)))

  def fromByUnfold(n: Int) = unfold(n)(x => Some((x, x + 1)))

  def constantByUnfold(n: Int) = unfold(n)(x => Some((x, x)))

  def onesByUnfold = unfold(1)(_ => Some((1, 1)))

  def mapByUnfold[A, B](a: Stream[A])(f: A => B): Stream[B] =
    unfold(a)(x => {
      x match {
        case EmptyStream => None
        case Cons(h, t) => Some(f(h()), t())
      }
    })

  def takeByUnfold[A](a: Stream[A], n: Int): Stream[A] =
    unfold((a, n))(x => {
      if (x._2 == 0) None
      else {
        x._1 match {
          case EmptyStream => None
          case Cons(h, t) => Some(h(), (t(), x._2 - 1))
        }
      }
    })

  def takeWhileByUnfold[A](a: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(a)(x => {
      x match {
        case EmptyStream => None
        case Cons(h, t) => if (!p(h())) None else Some(h(), t())
      }
    })

  def zip[A, B](as: Stream[A], bs: Stream[B]): Stream[(A, B)] =
    unfold((as, bs))(x => {
      x match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
        case _ => None
      }
    })

  def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((as, bs))(x => {
      x match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (EmptyStream, EmptyStream) => None
        case (Cons(h1, t1), EmptyStream) => Some((Some(h1()), None), (t1(), EmptyStream))
        case (EmptyStream, Cons(h2, t2)) => Some((None, Some(h2())), (EmptyStream, t2()))
      }
    })

  def startWith[A, B](as: Stream[A], bs: Stream[B]): Boolean =
    zip(as, bs).map(_._2).toList == bs.toList

  def tails[A](as: Stream[A]): Stream[Stream[A]] =
    as match {
      case EmptyStream => Stream(EmptyStream)
      case Cons(_, t) => Stream.cons(as, tails(t()))
    }

  def hasSubSequence[A](as: Stream[A], bs: Stream[A]): Boolean =
    tails(as).exists(startWith(_, bs))

}


object Chapter5 extends App {

  // test 5.1
  assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  assert(Stream.empty.toList == Nil)
  assert(Stream("a", "b", "c").toList == List("a", "b", "c"))

  // test 5.2
  assert(Stream(1, 2, 3).take(0).toList == Nil)
  assert(Stream(1, 2, 3).take(1).toList == List(1))
  assert(Stream(1).take(2).toList == List(1))
  assert(Stream(1, 2, 3).drop(2).toList == List(3))
  assert(Stream(1).drop(1).toList == Nil)
  assert(Stream(1).drop(2).toList == Nil)

  // test 5.3
  assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))
  assert(Stream(1, 2, 3).takeWhile(_ > 4).toList == Nil)
  assert(Stream(1, 2, 3).takeWhile(_ < 4).toList == List(1, 2, 3))

  // test 5.4
  assert(Stream(1, 2, 3).forAll(_ > 0) == true)

  // test 5.5
  assert(Stream(1, 2, 3).takeWhile2(_ < 3).toList == List(1, 2))
  assert(Stream(1, 2, 3).takeWhile2(_ > 4).toList == Nil)
  assert(Stream(1, 2, 3).takeWhile2(_ < 4).toList == List(1, 2, 3))

  // test 5.6
  assert(Stream(1, 2, 3).headOption == Some(1))
  assert(Stream.empty.headOption == None)

  // test 5.7
  assert(Stream(1, 2, 3).map(_ + "").toList == List("1", "2", "3"))
  assert(Stream(1, 2, 3).filter(_ < 3).toList == List(1, 2))
  assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
  assert(Stream(1, 2, 3).flatMap(x => Stream(x + 1, x + 2)).toList == List(2, 3, 3, 4, 4, 5))

  // test 5.8
  assert(Stream.constant(1).take(3).toList == List(1, 1, 1))

  // test 5.9
  assert(Stream.from(1).take(3).toList == List(1, 2, 3))

  // test 5.10
  assert(Stream.fibs(0, 1).take(5).toList == List(0, 1, 1, 2, 3))

  // test 5.11 and 5.12
  assert(Stream.fibsByUnfold(0, 1).take(5).toList == List(0, 1, 1, 2, 3))
  assert(Stream.fromByUnfold(1).take(3).toList == List(1, 2, 3))
  assert(Stream.constantByUnfold(1).take(3).toList == List(1, 1, 1))
  assert(Stream.onesByUnfold.take(3).toList == List(1, 1, 1))

  // test 5.13
  assert(Stream.mapByUnfold(Stream(1, 2, 3))(_ + 1).toList == List(2, 3, 4))
  assert(Stream.takeByUnfold(Stream(1, 2, 3), 2).toList == List(1, 2))
  assert(Stream.takeWhileByUnfold(Stream(1, 2, 3))(_ < 3).toList == List(1, 2))
  assert(Stream.zip(Stream(1, 2, 3), Stream("a", "b", "c")).toList == List((1, "a"), (2, "b"), (3, "c")))
  assert(Stream.zip(Stream(1, 2, 3), Stream("a", "b")).toList == List((1, "a"), (2, "b")))
  assert(Stream.zipAll(Stream(1, 2), Stream(1)).toList == List((Some(1), Some(1)), (Some(2), None)))

  // test 5.14
  assert(Stream.startWith(Stream(1, 2, 3), Stream(1, 2)))
  assert(Stream.startWith(Stream(1, 2, 3), Stream(1)))
  assert(Stream.startWith(Stream(1, 2, 3), EmptyStream))

  // test 5.15
  assert(Stream.tails(Stream(1, 2, 3)).toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), Nil))
  assert(Stream.hasSubSequence(Stream(1, 2, 3), Stream(2, 3)))

  // test 5.16
  assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  assert(Stream(1, 2, 3).scanRight(1)(_ * _).toList == List(6, 6, 3, 1))
  assert(Stream(1, 2, 3).scanRight(0)(_ - _).toList == List(2, -1,3, 0))
}
