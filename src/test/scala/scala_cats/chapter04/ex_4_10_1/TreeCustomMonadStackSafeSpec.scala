package scala_cats.chapter04.ex_4_10_1

import scala_cats.UnitSpec
import scala_cats.chapter04.CustomMonad._
import scala_cats.chapter04.ex_4_10_1.Tree.{Branch, Leaf, Tree, branch, leaf}
import scala_cats.chapter04.ex_4_10_1.TreeCustomMonadStackSafe._

class TreeCustomMonadStackSafeSpec extends UnitSpec {
  ignore can "does not terminate" in {
    def tree(a: Int): Tree[Int] =
      if (a < 1)
        Leaf(a)
      else
        Branch(tree(a - 1), tree(a - 2))

    assertThrows[StackOverflowError] {
      retry[Tree, Int](3)(tree)
    }
  }

  private val inputTree = branch(leaf(100), leaf(200))

  private val expectedTree = branch(
    branch(leaf(99), leaf(101)),
    branch(leaf(199), leaf(201))
  )

  import cats.syntax.flatMap._ // for flatMap

  "tree" can "flatMap" in {
    assert(inputTree.flatMap(x => branch(leaf(x - 1), leaf(x + 1))) == expectedTree)
  }

  import cats.syntax.functor._ // for map

  "tree" can "support for comprehension" in {
    assert((for {
      a <- inputTree
      b <- branch(leaf(a - 1), leaf(a + 1))
    } yield b) == expectedTree)

    assert((for {
      a <- inputTree
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c) ==
      branch(
        branch(
          branch(leaf(89), leaf(91)),
          branch(leaf(109), leaf(111))
        ),
        branch(
          branch(leaf(189), leaf(191)),
          branch(leaf(209), leaf(211))
        )
      )
    )
  }
}