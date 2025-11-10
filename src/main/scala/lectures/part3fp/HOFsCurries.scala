package lectures.part3fp

object HOFsCurries extends App {
  /*
    1.  Expand MyList
        - foreach method A => Unit
          [1,2,3].foreach(x => println(x))

        - sort function ((A, A) => Int) => MyList
          [1,2,3].sort((x, y) => y - x) => [3,2,1]

        - zipWith (list, (A, A) => B) => MyList[B]
          [1,2,3].zipWith([4,5,6], x * y) => [1 * 4, 2 * 5, 3 * 6] = [4,10,18]

        - fold(start)(function) => a value
          [1,2,3].fold(0)(x + y) = 6

    2.  toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
        fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int

    3.  compose(f,g) => x => f(g(x))
        andThen(f,g) => x => g(f(x))
   */

  def toCurry(f: (x: Int, y: Int) => Int): (Int => Int => Int) =
    x => y => f(x, y)

  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x, y) => f(x)(y)

  def compose[A,B,C](f: A => B, g: C => A): C => B =
    x => f(g(x))

  def andThen[A,B,C](f: A => B, g: B => C): A => C =
    x => g(f(x))

  val curriedAdd = toCurry(_ + _)
  val result1 = curriedAdd(2)
  println(result1(5))
  val uncurriedAdd = fromCurry(curriedAdd)
  println(uncurriedAdd(2, 3))

  val double: Int => Int = x => x * 2
  val increment: Int => Int = x => x + 1
  val composed = compose(double, increment)
  println(composed(3))
  val atTheEnd = andThen(double, increment)
  println(atTheEnd(3))

}
