package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {

  /*
    Exercises:
    1.  Concatenate a string n times
    2.  IsPrime function tail recursive
    3.  Fibonacci function, tail recursive.
   */


  private def concatenateString(string: String, number: Int): String = {
    @tailrec
    def concatenateStringTailRec(string: String, number: Int, accumulator: String): String = {
      if (number <= 0) accumulator
      else concatenateStringTailRec(string, number - 1, accumulator + string)
    }
    concatenateStringTailRec(string, number, "")
  }

  private def isPrime(number: Int): Boolean = {
    @tailrec
    def isPrimeTailRec(aux: Int, accumulator: Boolean): Boolean = {
      if (aux <= 1 || !accumulator) accumulator
      else isPrimeTailRec(aux - 1, number % aux != 0)
    }
    isPrimeTailRec(number / 2, true)
  }

  def fibonacci(number: Int): Int = {
    @tailrec
    def fibonacciTailRec(aux: Int, actualFibo: Int, nextFibo: Int): Int = {
      if (aux >= number) actualFibo
      else fibonacciTailRec(aux + 1, nextFibo, actualFibo + nextFibo)
    }
    fibonacciTailRec(0, 0, 1)
  }

  println(concatenateString("Matias", 10))
  println(isPrime(29))
  println(fibonacci(10))

}
