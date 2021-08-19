package scala_cats.chapter04.ex_4_8_3

import scala_cats.UnitSpec
import Db._

class DbSpec extends UnitSpec {
  "findUsername" can "return users name if matches" in {
    assert(findUsername(1).run(db) == Some("dade"))
  }

  it can "return None if no match" in {
    assert(findUsername(4).run(db) == None)
  }

  "check password" can "return true if password matches" in {
    assert(checkPassword("kate", "acidburn").run(db))
  }

  it can "return false if password does not match" in {
    assert(! checkPassword("kate", "acidburnz").run(db))
  }

  it can "return false if user cannot be found" in {
    assert(! checkPassword("katie", "acidburn").run(db))
  }

  "check login" can "return true if password matches" in {
    assert(checkLogin(2, "acidburn").run(db))
    assert(checkLoginTextbook(2, "acidburn").run(db))
  }

  it can "return false if password does not match" in {
    assert(! checkLogin(2, "acidburnz").run(db))
    assert(! checkLoginTextbook(2, "acidburnz").run(db))
  }

  it can "return false if user cannot be found" in {
    assert(! checkLogin(4, "acidburn").run(db))
    assert(! checkLoginTextbook(4, "acidburn").run(db))
  }

  it can "return false if wrong user found" in {
    assert(! checkLogin(3, "acidburn").run(db))
    assert(! checkLoginTextbook(3, "acidburn").run(db))
  }
}