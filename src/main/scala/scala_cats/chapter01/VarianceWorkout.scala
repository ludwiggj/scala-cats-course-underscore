package scala_cats.chapter01

import scala_cats.chapter01.json.{JsString, Json}

object VarianceWorkout {
  sealed trait Shape
  case class Circle(radius: Double) extends Shape
  val circles: List[Circle] = List(Circle(0.5), Circle(2.3))
  val shapes: List[Shape] = circles

  trait JsonWriter[-A] {
    def write(value: A): Json
  }

  val shape: Shape = new Shape {}
  val circle: Circle = Circle(1.3)
  val shapeWriter: JsonWriter[Shape] = (_: Shape) => JsString("Shape")
  val circleWriter: JsonWriter[Circle] = (_: Circle) => JsString("Circle")

  def format[A](value: A, writer: JsonWriter[A]): Json = writer.write(value)

  def main(args: Array[String]): Unit = {
    // Shape writer can write circles and shapes, as all circles are shapes
    println(format(shape, shapeWriter))
    println(format(circle, shapeWriter))

    // Circle writer can write circle
    println(format(circle, circleWriter))

    // But cannot write a shape
    // println(format(shape, circleWriter))
  }
}
