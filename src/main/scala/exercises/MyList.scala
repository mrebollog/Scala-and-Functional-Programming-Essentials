package exercises

abstract class MyList {
  /*
     head = first element of  the  list
     tail = remainder of the list
     isEmpty = is this list empty
     add(int) => new list with this element added
     toString => a string representation of the list
   */
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(value: Int): MyList
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object EmptyList extends MyList{
  def head: Int = throw new NoSuchElementException()
  def tail: MyList = throw new NoSuchElementException()
  def isEmpty: Boolean = true
  def add(value: Int): MyList = new NotEmptyList(value, EmptyList)
  def printElements: String = ""
}

class NotEmptyList(h: Int, t: MyList) extends MyList{
  def head: Int = h
  def tail: MyList = t
  def isEmpty: Boolean = false
  def add(value: Int): MyList = new NotEmptyList(value, this)
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
