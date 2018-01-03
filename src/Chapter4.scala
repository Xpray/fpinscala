import scala.annotation.tailrec

/**
  * Created by xpray on 2017/3/24.
  */


trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {

  override def map[B](f: (Nothing) => B): Option[B] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None

}

case class Some[+A](elem: A) extends Option[A] {

  override def map[B](f: (A) => B): Option[B] = Some(f(elem))

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(elem)

  override def getOrElse[B >: A](default: => B): B = elem

  override def orElse[B >: A](ob: => Option[B]): Option[B] = Some(elem)

  override def filter(f: (A) => Boolean): Option[A] = if (f(elem)) Some(elem) else None

}

object Chapter4 {



  //4.2
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.map(x => math.pow(x - mean(xs).getOrElse(0.0), 2)).sum)
  }

  // 4.3
  def list[A, B](f: A => B): Option[A] =>  Option[B] = _ map(f)
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x1), Some(x2)) => Some(f(x1, x2))
      case _ => None
    }
  }

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def g(acc: Option[List[A]], curr: List[Option[A]]): Option[List[A]] = {
      curr match {
        case Nil => acc
        case Some(e) :: tail => g(acc.map(_ ::: List(e)), tail)
        case None :: tail => None
      }
    }
    g(Some(Nil), a)
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def g(acc: Option[List[B]], curr: List[A]): Option[List[B]] = {
      curr match {
        case Nil => acc
        case head :: tail => {
          f(head) match {
            case Some(e) => g(acc.map(_ ::: List(e)), tail)
            case _ => None
          }
        }
      }
    }
    g(Some(Nil), a)
  }



  def main(args: Array[String]): Unit = {

    assert(Some(5).map(_ + 1) == Some(6))
    assert(Some("3").map(_ + "x") == Some("3x"))

    assert(Some(1).flatMap(_ => None) == None)

    assert(Some(1).getOrElse(3) == 1)
    assert(None.getOrElse(3) == 3)

    assert(Some(3).orElse(None) == Some(3))
    assert(None.orElse(Some(2)) == Some(2))

    assert(Some(3).filter(_<4) == Some(3))
    assert(Some(3).filter(_<3) == None)
    assert(None.filter((_)=>false) == None)

    // test 4.3
    assert(map2(Some(1), Some(3))(_ + _) == Some(4))
    assert(map2(Some(1), None: Option[Int])(_ * _) == None)


    // test 4.4
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(sequence(List(Some(1), Some(2), None)) == None)

    // test 4.5
    assert(traverse(List(1, 2, 3))(x => {if (x < 2) None else Some(x)}) == None)
    assert(traverse(List(1, 2, 3))(x => Some(x + 1)) == Some(List(2, 3, 4)))
  }


}
