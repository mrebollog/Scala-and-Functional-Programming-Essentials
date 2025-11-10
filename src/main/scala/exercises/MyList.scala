package exercises

abstract class MyList[+A] {
  /*
     head = first element of  the  list
     tail = remainder of the list
     isEmpty = is this list empty
     add(int) => new list with this element added
     toString => a string representation of the list
   */

  // expand MyList to be generic

  /*
    Exercises:
    1.  Generic trait MyPredicate[-T] with a little method test(T) => Boolean
    2.  Generic trait MyTransformer[-A, B] with a method transform(A) => B
    3.  MyList:
        - map(transformer) => MyList
        - filter(predicate) => MyList
        - flatMap(transformer from A to MyList[B]) => MyList[B]

        class EvenPredicate extends MyPredicate[Int]
        class StringToIntTransformer extends MyTransformer[String, Int]

        [1,2,3].map(n * 2) = [2,4,6]
        [1,2,3,4].filter(n % 2) = [2,4]
        [1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,4]
   */
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](value: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: A => B): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def ++[B >: A](list: MyList[B]): MyList[B]
}

case object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException()
  def tail: MyList[Nothing] = throw new NoSuchElementException()
  def isEmpty: Boolean = true
  def add[B >: Nothing](value: B): MyList[B] = new NotEmptyList(value, EmptyList)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyList[Nothing] = EmptyList
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = EmptyList
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[Nothing] = EmptyList
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
}

case class NotEmptyList[+A](h: A, t: MyList[A]) extends MyList[A]{
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](value: B): MyList[B] = new NotEmptyList(value, this)
  def printElements: String =
    if (t.isEmpty) "" + h
    else s"$h ${t.printElements}"


  def map[B](transformer: A => B): MyList[B] = {
    new NotEmptyList(transformer(h), t.map(transformer))
  }
  def filter(predicate: A => Boolean): MyList[A] = {
    if (predicate(h)) new NotEmptyList(h, t.filter(predicate))
    else t.filter(predicate)
  }
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
  def ++[B >: A](list: MyList[B]): MyList[B] = new NotEmptyList(h, t ++ list)
}

object ListTest extends App {
  private val list = new NotEmptyList(1, new NotEmptyList(2, new NotEmptyList(3, EmptyList)))
  println(list.tail.head)
  println(list.add(22).head)
  println(list.isEmpty)

  println(list.map((element: Int) => element + 13).toString)

  println(list.filter((element: Int) => element % 2 == 0).toString)

  println(list.flatMap((element: Int) => new NotEmptyList(element, new NotEmptyList(element * 2, EmptyList))).toString)
}
