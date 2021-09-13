package kleislis.take1

// pass the logs from one function execution to the next as a parameter
object Solution02 {
  def main(args: Array[String]): Unit = {
    val negate_1: (Boolean, String) => (Boolean, String) = { (x: Boolean, log: String) =>
      (!x, log + s"negate_1 called with $x\n")
    }

    val negate_2: (Boolean, String) => (Boolean, String) = { (x: Boolean, log: String) =>
      (!x, log + s"negate_2 called with $x\n")
    }

    val negate_1_Result = negate_1(true, "")
    val negate_2_Result = negate_2(false, negate_1_Result._2)
    println("LOG: \n" + negate_2_Result._2)
  }
}
