package circe

class CirceWorkout {
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
}