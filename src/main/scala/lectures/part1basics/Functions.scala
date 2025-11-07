package lectures.part1basics

import scala.annotation.tailrec

object Functions extends App {

  /*
  Exercises:
  1.  A greeting function (name, age) => "Hi, my name is $name, and I am $age years old."
  2.  Factorial function 1 * 2 * 3 * . * n
  3.  A Fibonacci function
      f(1) = 1
      f(2) = 1
      f(n) = f(n - 1) + f(n - 2)
  4.  Tests if a number is prime.
  */

  private def greeting(name: String, age: Int): Unit =
    println(s"Hi, my name is $name, and I am $age years old.")

  private def factorial(number: Int): Int = {
    if (number <= 1) 1
    else number * factorial(number - 1)
  }

  private def fibonacci(number: Int): Int = {
    if (number == 0) 0
    else if (number <= 2) 1
    else fibonacci(number - 1) + fibonacci (number - 2)
  }

  private def isPrime(number: Int): Boolean = {
    @tailrec
    def auxIsPrime(aux: Int): Boolean = {
      if (aux <= 1) true
      else number % aux != 0 && auxIsPrime(aux - 1)
    }
    auxIsPrime(number / 2)
  }

  greeting("Matias", 22)
  println(factorial(30))
  println(fibonacci(9))
  println(isPrime(29))
}
