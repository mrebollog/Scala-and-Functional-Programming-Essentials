package lectures.part3fp

object WhatsAFunction extends App {
  /*
    1.  a function which takes 2 strings and concatenates them
    2.  transform the MyPredicate and MyTransformer into function types
    3.  define a function which takes an int and returns another function which takes an int and returns an int
        - what's the type of this function
        - how to do it
   */

  def concatenator = new Function2[String, String, String]{
    override def apply(string1: String, string2: String): String = string1 + string2
  }

  def function = new Function1[Int, Function1[Int, Int]]{
    override def apply(x: Int): Function1[Int, Int] = new Function1[Int, Int]{
      override def apply(y: Int): Int = y + x
    }
  }

  println(concatenator("Matias", "Matias"))
  println(function(1)(2))
}
