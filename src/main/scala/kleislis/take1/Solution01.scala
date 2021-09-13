package kleislis.take1

// Logging using a global variable
object Solution01 {
  def main(args: Array[String]): Unit = {

    var logAcc: String = ""

    val simple_1: Boolean => Boolean = { x =>
      logAcc = logAcc + s"simple_1 called with $x\n"
      x
    }

    val simple_2: Boolean => Boolean = { x =>
      logAcc = logAcc + s"simple_2 called with $x\n"
      x
    }

    simple_1(true)
    simple_2(false)
    println("LOG: \n" + logAcc)
  }
}
