package lectures.part2oop

object Exceptions extends App {

  /*
      1.  Crash your program with an OutOfMemoryError
      2.  Crash with SOError
      3.  PocketCalculator
          - add(x,y)
          - subtract(x,y)
          - multiply(x,y)
          - divide(x,y)

          Throw
            - OverflowException if add(x,y) exceeds Int.MAX_VALUE
            - UnderflowException if subtract(x,y) exceeds Int.MIN_VALUE
            - MathCalculationException for division by 0
     */
  //OOMError
  val lista = List.fill(Int.MaxValue)(Array.fill(1000)(0))

  //SOError
  def infinito(): Int = {
    val n = 1 + infinito()
    n
  }
  infinito()

  class OverflowException extends Exception
  class UnderflowException extends Exception
  class DivisionByZeroException extends Exception

  object PocketCalculator {

    def add(x: Int, y: Int): Int = {
      val result = x + y
      if (x > 0 && y > 0 && result < 0) throw new OverflowException
      else if (x < 0 && y < 0 && result > 0) throw new UnderflowException
      else result
    }

    // I could check overflow or underflow exceptions here too
    def substract(x: Int, y: Int): Int = {
      x - y
    }

    // I could check overflow or underflow exceptions here too
    def multiply(x: Int, y: Int): Int = {
      x * y
    }

    def divide(x: Int, y: Int): Int = {
      if (y == 0) throw new DivisionByZeroException
      else x / y
    }

    println(PocketCalculator.add(Int.MaxValue, 10))
    println(PocketCalculator.divide(2, 0))
  }
}
