package lectures.part2oop

import scala.language.postfixOps

object MethodNotations extends App {

  /*
    1.  Overload the + operator
        mary + "the rockstar" => new person "Mary (the rockstar)"

    2.  Add an age to the Person class
        Add a unary + operator => new person with the age + 1
        +mary => mary with the age incrementer

    3.  Add a "learns" method in the Person class => "Mary learns Scala"
        Add a learnsScala method, calls learns method with "Scala".
        Use it in postfix notation.

    4.  Overload the apply method
        mary.apply(2) => "Mary watched Inception 2 times"
   */

  class Person(val name: String, val age: Int) {
    def +(alias: String): Person = new Person(s"$name ($alias)", age)
    def unary_+ : Person = new Person(name, age + 1)
    def learns(topic: String): String = s"$name learns $topic"
    def learnsScala: String = this learns "Scala"
    def apply(number: Int): String = s"$name watched Inception $number times"
  }

  private val person = new Person("Matias", 22)
  println((person + "el tute").name)
  println((+person).age)
  println(person learns "Java")
  println(person learnsScala)
  println(person(3))
}
