package scala_cats.chapter05

import scala_cats.UnitSpec
import scala_cats.chapter05.MonadGlue.addAll

class MonadGlueSpec extends UnitSpec {
  "addAll" can "log a success" in {
    assert(addAll("1", "2", "3").run == ((List("Read 1", "Read 2", "Read 3"), Some(6))))
  }

  it can "log a failure" in {
    assert(addAll("1", "a", "3").run == ((List("Read 1", "Failed on a"), None)))
  }
}