
class Par[A]

object Par {
  def unit[A](a: A): Par[A] = ???
  // 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???
}

object Chapter7 extends App {

}

