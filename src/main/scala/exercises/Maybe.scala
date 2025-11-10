package exercises

abstract class Maybe[+A] {

  def map[B](f: A => B): Maybe[B]
  def flatMap[B](f: A => Maybe[B]): Maybe[B]
  def filter(p: A => Boolean): Maybe[A]
}

case object Empty extends Maybe[Nothing] {
  def map[B](f: Nothing => B): Maybe[B] = Empty
  def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = Empty
  def filter(p: Nothing => Boolean): Maybe[Nothing] = Empty
}

case class One[+A](value: A) extends Maybe[A] {

  def map[B](f: A => B): Maybe[B] = One(f(value))
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(value)
  def filter(p: A => Boolean): Maybe[A] = {
    if (p(value)) this
    else Empty
  }

}

object MaybeTest extends App {
  val value = One(23)
  println(value.map(_ * 10))
  println(value.flatMap(x => One("The number " + value)))
  println(value.filter(_ % 10 == 0))

}
