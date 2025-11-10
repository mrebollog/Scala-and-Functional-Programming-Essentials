package lectures.part2oop

object OOBasics extends App {

  private val author = new Writer("Matias", "Rebollo", 2003)
  private val imposter = new Writer("Matias", "Rebollo", 2003)
  private val novel = new Novel("Yo, Matias", 2018, author)

  println(novel.authorAge)
  println(novel.isWrittenBy(author))
  println(novel.isWrittenBy(imposter))

  val counter = new Counter(10)
  println(counter.currentCount)
  println(counter.increment.currentCount)
  println(counter.decrement.currentCount)
  println(counter.increment(20).currentCount)
  println(counter.decrement(50).currentCount)



}
  /*
    Novel and a Writer

    Writer: first name, surname, year of birth
      - method fullname

    Novel: name, year of release, author
    - authorAge (at the yer of release)
    - isWrittenBy(author)
    - copy (new year of release) = new instance of Novel
   */

class Writer(firstName: String, surname: String, val year: Int) {
  def fullname: String = firstName + " " + surname
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge: Int = yearOfRelease - author.year
  def isWrittenBy(otherAutor: Writer): Boolean = author == otherAutor
  def copy (newYearOfRelease: Int): Novel = new Novel(name, newYearOfRelease, author)
}


  /*
    Counter class
      - receives an int value
      - method current count
      - method to increment/decrement => new Counter
      - overload inc/dec to receive an amount
   */

class Counter(value: Int) {
  def currentCount: Int = value
  def increment: Counter = new Counter(value + 1)
  def decrement: Counter = new Counter(value - 1)
  def increment(amount: Int) = new Counter(value + amount)
  def decrement(amount: Int) = new Counter(value - amount)
}

