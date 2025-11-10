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
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](value: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException()
  def tail: MyList[Nothing] = throw new NoSuchElementException()
  def isEmpty: Boolean = true
  def add[B >: Nothing](value: B): MyList[B] = new NotEmptyList(value, EmptyList)
  def printElements: String = ""
}

class NotEmptyList[+A](h: A, t: MyList[A]) extends MyList[A]{
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](value: B): MyList[B] = new NotEmptyList(value, this)
  def printElements: String =
    if (t.isEmpty) "" + h
    else s"$h ${t.printElements}"
}

object ListTest extends App {
  private val list = new NotEmptyList(1, new NotEmptyList(2, new NotEmptyList(3, EmptyList)))
  println(list.tail.head)
  println(list.add(22).head)
  println(list.isEmpty)
}
