package circe

// https://circe.github.io/circe/

// jacqueline.hubbard Today at 08:41
// Hi, does anyone have an example of a circe decoder where you can supply it one of the values directly? I’m trying to create a decoder method where I can pass in one of the values in the params as this value is given in the rabbitMQ msg and not when decoding the API response (which is where we get all the rest of the values from).
// I have written a method that I feel should work but I’m not sure how to include this method where its needed implicitly for the circe decoder. Thanks (edited)
//
//Adam King  5 hours ago
// naive question but why not just add the value on after decoding? ie MyClass(provided: RabbitValue, decoded: ApiResponse) which could be as simple as .map(provided, _)
//
// jacqueline.hubbard  5 hours ago
// :thinking_face: I’ll try that, thank you!! :slightly_smiling_face:
//
// jack.wheatley  4 hours ago
// @jacqueline.hubbard another way of doing thing is creating a Decoder[Value => CaseClass], and then adding the value by doing

// import cats.syntax.all._
// decoder.decodeJson(json).mapApply(value) : Result[CaseClass]
//
// jack.wheatley  4 hours ago
//here is a full example of a decoder like this:

// implicit val decoder: Decoder[ItvId => PurchaseRequest] = Decoder.instance { cur =>
//     for {
//       purchaseToken    <- cur.get[PurchaseToken]("purchaseToken")
//       subscriptionItem <- cur.get[SubscriptionItem]("subscriptionItem")
//     } yield (itvId: ItvId) => PurchaseRequest(itvId, subscriptionItem, purchaseToken)
//   }
//
// jacqueline.hubbard  4 hours ago
// Nice!! Thank you @jack.wheatley :slightly_smiling_face:

import io.circe.syntax._
import io.circe.parser._
import io.circe._

// Examples from https://www.scala-exercises.org/circe/Encoding%20and%20decoding
object CirceWorkout {
  def showNoSpaces(j: Json): Unit = println(j.noSpaces)

  def showSpaces2(j: Json): Unit = println(j.spaces2)

  def showSpaces4(j: Json): Unit = println(j.spaces4)

  def json(): Unit = {
    showNoSpaces(Json.fromString("scala exercises"))
    showNoSpaces(Json.fromDouble(1).get)
    showNoSpaces(Json.fromBoolean(true))

    val fieldList = List(
      ("key1", Json.fromString("value1")),
      ("key2", Json.fromInt(1))
    )

    showNoSpaces(Json.fromFields(fieldList))
    showSpaces2(Json.fromFields(fieldList))
    showSpaces4(Json.fromFields(fieldList))

    showNoSpaces(Json.fromFields(List(("key", Json.fromString("value")))))

    val recursiveFieldList = List(
      ("name", Json.fromString("sample json")),
      ("data", Json.fromFields(List(("done", Json.fromBoolean(false)))))
    )

    showSpaces2(Json.fromFields(recursiveFieldList))

    showNoSpaces(Json.arr(Json.fromFields(List(("x", Json.fromInt(1))))))

    showNoSpaces(Json.fromValues(List(Json.fromFields(List(("x", Json.fromInt(1)))))))

    val jsonArray: Json = Json.fromValues(
      List(
        Json.fromFields(List(("field1", Json.fromInt(1)))),
        Json.fromFields(List(
          ("field1", Json.fromInt(200)),
          ("field2", Json.fromString("Having circe in Scala Exercises is awesome")))
        )
      )
    )

    showSpaces2(jsonArray)

    def transformJson(jsonArray: Json): Json =
      jsonArray mapArray { (oneJson: Vector[Json]) => oneJson.init }

    showNoSpaces(transformJson(jsonArray))
  }

  def traversingAndModifying(): Unit = {
    val json: String =
      """{
         "id": "c730433b-082c-4984-9d66-855c243266f0",
         "name": "Foo",
         "counts": [1, 2, 3],
         "values": {
         "bar": true,
         "baz": 100.001,
         "qux": ["a", "b"]
         }
         }"""

    val doc: Json = parse(json).getOrElse(Json.Null)
    showSpaces2(doc)

    val cursor: HCursor = doc.hcursor

    val baz: Decoder.Result[Double] = cursor.downField("values").downField("baz").as[Double]
    println(baz)

    val baz2 = cursor.downField("values").get[Double]("baz")
    println(baz2)

    val qux1 = cursor.downField("values").downField("qux").downArray.as[String]
    println(qux1)

    val qux2 = cursor.downField("values").downField("qux").downArray.right.as[String]
    println(qux2)

    val reversedNameCursor: ACursor = cursor.downField("name").withFocus(_.mapString(_.reverse))

    println(reversedNameCursor.as[String])

    showSpaces2(reversedNameCursor.top.get)
  }

  // Less magical code...
  case class User(id: Long, firstName: String, lastName: String)

  object User {
    implicit val decodeUser: Decoder[User] =
      Decoder.forProduct3("id", "first_name", "last_name")(User.apply)

    implicit val encodeUser: Encoder[User] =
      Encoder.forProduct3("id", "first_name", "last_name")(u =>
        (u.id, u.firstName, u.lastName)
      )
  }

  case class Person(name: String)

  case class Greeting(salutation: String, person: Person, exclamationMarks: Int)

  def encodingAndDecoding(): Unit = {
    // Encode data to json
    val intsJson: Json = List(1, 2, 3).asJson
    showNoSpaces(intsJson)

    // Decode json to data
    println(intsJson.as[List[Int]])

    // Doesn't compile
    // println("[1,2,3]".as[List[Int]])

    // Decode from json string representation to data
    val decodeList: Either[Error, List[Int]] = decode[List[Int]]("[1,2,3]")
    println(decodeList)

    // Parse from json string representation to json
    val parsedList: Either[ParsingFailure, Json] = parse("[1,2,3]")
    showNoSpaces(parsedList.toOption.get)

    // Decode via parse and as
    println(parse("[1,2,3]").flatMap(_.as[List[Int]]))

    val decodeUser = decode[User]("""{"id": 1, "first_name": "Graeme", "last_name": "Ludwig"}""")
    println(decodeUser)

    // encode user
    showSpaces2(User(2, "Audrey", "Ludwig").asJson)

    // Automatic derivation
    import io.circe.generic.auto._
    val greetingJson = Greeting("Hey", Person("Chris"), 3).asJson
    showSpaces2(greetingJson)
    println(greetingJson.hcursor.downField("person").downField("name").as[String])
  }

  class Thing(val name: String, val count: Int) {
    override def toString: String = s"Thing(name:$name, count:$count)"
  }

  object Thing {
    def apply(name: String, count: Int) = new Thing(name, count)
  }

  object ThingEncoders1 {
    implicit val encodeThing: Encoder[Thing] =
      Encoder.forProduct2("name", "count")(t =>
        (t.name, t.count)
      )

    implicit val decodeThing: Decoder[Thing] =
      Decoder.forProduct2("name", "count")(Thing.apply)
  }

  object ThingEncoders2 {
    implicit val encodeThing: Encoder[Thing] = (a: Thing) => {
      val fieldList = List(
        ("name", Json.fromString(a.name)),
        ("count", Json.fromInt(a.count))
      )
      Json.fromFields(fieldList)
    }

    import cats.syntax.apply._ // for mapN

    implicit val decodeThing: Decoder[Thing] = (c: HCursor) => {
      (c.get[String]("name"), c.get[Int]("count")).mapN(Thing.apply)
    }
  }

  object ThingEncoders3 {
    implicit val encodeThing: Encoder[Thing] = t =>
      Json.obj(
        "name" -> t.name.asJson,
        "count" -> t.count.asJson
      )

    implicit val decodeThing: Decoder[Thing] = c =>
      for {
        name <- c.get[String]("name")
        count <- c.get[Int]("count")
      } yield Thing.apply(name, count)
  }

  def customCodecs(): Unit = {
    val thing = new Thing("thing 1", 1)
    val thingJson = """{"name": "thing 2", "count": 1}"""

    {
      import ThingEncoders1._
      // Encode
      showNoSpaces(thing.asJson)

      // Decode
      println(decode[Thing](thingJson))
    }
    {
      import ThingEncoders2._
      // Encode
      showNoSpaces(thing.asJson)

      // Decode
      println(decode[Thing](thingJson))
    }

    {
      import ThingEncoders3._

      // Encode
      showNoSpaces(thing.asJson)

      // Decode
      println(decode[Thing](thingJson))
    }
  }

  def main(args: Array[String]): Unit = {
    json()
    traversingAndModifying()
    encodingAndDecoding()
    customCodecs()
  }
}