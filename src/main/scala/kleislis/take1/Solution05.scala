package kleislis.take1

object Solution05 {
  def main(args: Array[String]): Unit = {
    val r = scala.util.Random

    val generate: Unit => Int = _ => r.nextInt(100)
    val process: Int => Int = v => (v * math.Pi).toInt
    val save: Int => Boolean = _ => true

    // Simplest approach is to pass values between functions
    val generated: Int = generate(())
    val processed: Int = process(generated)
    val saved: Boolean = save(processed)

    println(s"Result is: $saved")

    // Or could just inline the calls
    val combine_1: Unit => Boolean = _ => save(process(generate(())))
    println(s"Result 1 is: ${combine_1(())}")

    // We have functions like compose and andThen
    // this is a bit difficult to read as it reads from right to left
    val combine_2: Unit => Boolean = save compose process compose generate
    println(s"Result 2 is: ${combine_2(())}")

    // andThen version
    val combine_3: Unit => Boolean = generate andThen process andThen save
    println(s"Result 3 is: ${combine_3(())}")
  }
}
