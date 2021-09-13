package kleislis.take1

// Return a tuple as per solution 2, but just a Boolean parameter
// Defer the log concatenation to the point where we call our functions
object Solution03 {
  def main(args: Array[String]): Unit = {

    val negate_1: Boolean => (Boolean, String) = { (x: Boolean) =>
      (!x, s"negate_1 called with $x\n")
    }

    val negate_2: Boolean => (Boolean, String) = { (x: Boolean) =>
      (!x, s"negate_2 called with $x\n")
    }

    val negate_1_Result = negate_1(true)
    val negate_2_Result = negate_2(false)
    println(s"LOG: \n${negate_1_Result._2}${negate_2_Result._2}")
  }
}
