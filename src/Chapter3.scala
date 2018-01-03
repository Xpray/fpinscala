import scala.annotation.tailrec

/**
  * Created by xpray on 2017/3/15.
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case object Empty extends Tree[Nothing]


object Chapter3 {

  // 3.2
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => throw new RuntimeException("tail with Nil")
      case _ :: t => t
    }

  // 3.3
  def setHead[A](as: List[A], v: A): List[A] = {
    as match {
      case Nil => throw new RuntimeException("set head with Nil")
      case _ :: t => v :: t
    }
  }


  // 3.4
  def drop[A](as: List[A], n: Int): List[A] = {
    n match {
      case 0 => as
      case _ => {
        as match {
          case Nil => throw new RuntimeException("tail with Nil")
          case h :: t => drop(t, n - 1)
        }
      }
    }
  }

  // 3.5
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => as
      case h :: t => if (f(h)) dropWhile(t)(f) else as
    }
  }

  // 3.6
  def init[A](as: List[A]): List[A] = {
    as match {
      case Nil => throw new RuntimeException("delete last with Nil")
      case h :: Nil => Nil
      case h :: t => h :: init(t)
    }
  }



  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case h :: tail => f(h, foldRight(tail, z)(f))
    }

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  // 3.10 A tailrec foldLeft
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def g(list: List[A], acc: B): B =
      list match {
        case Nil => acc
        case head :: tail => g(tail, f(acc, head))
      }
    g(as, z)
  }

  // 3.11
  def sum(as: List[Int]) = foldLeft(as, 0)(_ + _)

  def product(as: List[Double]) = foldLeft(as, 1.0)(_ * _)

  def lengthByFoldLeft[A](as: List[A]) = foldLeft(as, 0)((x, y) => x + 1)


  // 3.12
  def reverseByFoldRight[A](as: List[A]) = foldRight(as, Nil: List[A])((x, y) => y ::: List(x))

  def reverseByFoldLeft[A](as: List[A]) = foldLeft(as, Nil: List[A])((x, y) => y :: x)

  // 3.13
  def foldRightByLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def g(list: List[A], acc: B): B =
      list match {
        case Nil => acc
        case head :: tail => g(tail, f(head, acc))
      }
    g(reverseByFoldLeft(as), z)
  }

  def foldLeftByRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def g(list: List[A], acc: B): B =
      list match {
        case Nil => acc
        case head :: tail => g(tail, f(acc, head))
      }
    g(reverseByFoldRight(as), z)
  }

  // 3.14
  def append1[A](as: List[A], elem: A) = foldLeft(as, List(elem))((x, y) => x ::: List(y))

  def append2[A](as: List[A], elem: A) = foldRight(as, List(elem))((x, y) => x :: y)

  // 3.15
  def appendList[A](as: List[A], bs: List[A]) = foldRight(as, bs)((x, y) => x :: y)

  def mergeLists[A](as: List[List[A]]): List[A] = {
    @tailrec
    def g(acc: List[A], curr: List[List[A]]): List[A] = {
      curr match {
        case Nil => acc
        case h :: t => g(appendList(acc, h), t)
      }
    }
    g(Nil, as)
  }

  // 3.16
  def addOne(as: List[Int]): List[Int] =
    as match {
      case Nil => Nil
      case h :: t => h + 1 :: addOne(t)
    }

  // 3.17
  def toString(as: List[Double]): List[String] =
    as match {
      case Nil => Nil
      case h :: t => h.toString :: toString(t)
    }

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case h :: t => f(h) :: map(t)(f)
    }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case h :: t => if(f(h)) h :: filter(t)(f) else filter(t)(f)
    }

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil => Nil
      case h :: t => f(h) ::: flatMap(t)(f)
    }

  // 3.21
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  // 3.22
  def listCombine(as: List[Int], bs: List[Int]): List[Int] =
    as match {
      case Nil => Nil
      case h :: t => h + bs.head :: listCombine(t, bs.tail)
    }


  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    as match {
      case Nil => Nil
      case h :: t => f(h, bs.head) :: zipWith(t, bs.tail)(f)
    }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def contains(as: List[A], bs: List[A]): Boolean = {
      if (as.length == bs.length) {
        as == bs
      } else {
        if (as.take(bs.length) == bs) true
        else contains(as.tail, bs)
      }
    }
    contains(sup, sub)
  }


  // 3.25
  def treeSize[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(_, left, right) => 1 + treeSize(left) + treeSize(right)
    }
  }


  // 3.26
  def treeMax(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(v, l, r) => v max treeMax(l) max treeMax(r)
    }
  }


  // 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(_, l, r) => (depth(l) max depth(r)) + 1
    }
  }

  // 3.28
  def treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), treeMap(l)(f), treeMap(r)(f))
    }
  }

  // 3.29
  def treeFold[A, B](tree: Tree[A], zl: B, zr: B)(f: (B, B, A) => B): B = {
    tree match {
      case Leaf(v) => f(zl, zr, v)
      case Branch(v, l, r) => f(treeFold(l, zl, zr)(f), treeFold(r, zl, zr)(f), v)
    }
  }

  def treeSizeByFold[A](tree: Tree[A]): Int = treeFold(tree, 0, 0)((x, y, _) => x + y + 1)

  def treeMaxByFold(tree: Tree[Int]): Int = treeFold(tree, Int.MinValue, Int.MinValue)((x, y, z) => x max y max z)

  def depthByFold[A](tree: Tree[A]): Int = treeFold(tree, -1, -1)((x, y, _) => (x max y) + 1)

  def treeMapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    treeFold(tree, Empty: Tree[B], Empty: Tree[B])(
      (x, y, z) => {
        if (x == Empty && y == Empty) Leaf(f(z))
        else Branch(f(z), x, y)
      }
    )

  // test
  def main(args: Array[String]): Unit = {

    // 3.2 test
    assert(tail(List(1, 2, 3)) == List(2, 3))

    // 3.3 test
    assert(setHead(List(1, 2, 3), 2) == List(2, 2, 3))

    // 3.4 test
    assert(drop(List(1, 2, 3), 2) == List(3))

    // 3.5 test
    assert(dropWhile(List(1, 2, 3))(_ < 3) == List(3))

    // 3.6
    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))

    // 3.9 test
    assert(length(List(1, 2, 3)) == 3)
    assert(length(Nil) == 0)

    // 3.11 test
    assert(sum(List(1, 2, 3)) == 6)
    assert(product(List(1.0, 3.0, 4.0)) == 12.0)
    assert(lengthByFoldLeft(List("3", "2", "1")) == 3)
    assert(lengthByFoldLeft(Nil) == 0)

    // 3.12 test
    assert(reverseByFoldRight(List(1, 2, 3)) == List(3, 2, 1))
    assert(reverseByFoldLeft(List("a", "b", "c")) == List("c", "b", "a"))

    // 3.13 test
    assert(foldRightByLeft(List(1, 2, 3), 0)(_ + _) == 6)
    assert(foldRightByLeft(List(3, 4, 5), 1)(_ * _) == 60)
    assert(foldLeftByRight(List(1, 2, 3), 0)(_ + _) == 6)
    assert(foldLeftByRight(List(3, 2, 1), 0)(_ - _) == -6)

    // 3.14 test
    assert(append1(List(1, 2, 3), 4) == List(4, 1, 2, 3))
    assert(append2(List(1, 2, 3), 4) == List(1, 2, 3, 4))

    // 3.15 test
    assert(mergeLists(List(List(1, 2, 3), List(4, 5, 6))) == List(1, 2, 3, 4, 5, 6))

    // 3.16 test
    assert(addOne(List(1, 2, 3)) == List(2, 3, 4))

    // 3.17 test
    assert(toString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))

    // 3.18 test
    assert(map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
    assert(map(List(1, 2, 3))(_ * 2) == List(2, 4, 6))

    // 3.19 test
    assert(filter(List(1, 2, 3, 4, 5))(_ > 3) == List(4, 5))
    assert(filter(List(1, 2, 3, 4, 5))(_ % 2 == 1) == List(1, 3, 5))

    // 3.20 test
    assert(flatMap(List(1, 2, 3))(x => List(-x, x)) == List(-1, 1, -2, 2, -3, 3))
    assert(flatMap(List(1, 2, 3))(x => List(s"x$x", s"y$x")) == List("x1", "y1", "x2", "y2", "x3", "y3"))

    // 3.21 test
    assert(filterByFlatMap(List(1, 2, 3, 4, 5))(_ > 3) == List(4, 5))
    assert(filterByFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 1) == List(1, 3, 5))

    // 3.22 test
    assert(listCombine(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))

    // 3.23 test
    assert(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + "+" + _) == List("1+4", "2+5", "3+6"))

    // 3.24 test
    assert(hasSubsequence(List(1, 2, 3), List(2, 3)) == true)
    assert(hasSubsequence(List(1), Nil) == true)
    assert(hasSubsequence(List(1, 2, 3), List(3, 4)) == false)



    val tree = Branch[Int](10, Leaf(1), Branch(4, Leaf(2), Leaf(3)))
    // 3.25 test
    assert(treeSize(tree) == 5)

    // 3.26 test
    assert(treeMax(tree) == 10)

    // 3.27 test
    assert(depth(tree) == 2)

    // 3.28 test
    val _tree = Branch[String]("10", Leaf("1"), Branch("4", Leaf("2"), Leaf("3")))
    assert(treeMap(tree)(_ + "") == _tree)


    // 3.29 test
    assert(treeSize(tree) == treeSizeByFold(tree))
    assert(treeMax(tree) == treeMaxByFold(tree))
    assert(depth(tree) == depthByFold(tree))
    assert(treeMap(tree)(_ + "") == treeMapByFold(tree)(_ + ""))
    //println(treeMapByFold(tree)(_ * 2))
  }

}
