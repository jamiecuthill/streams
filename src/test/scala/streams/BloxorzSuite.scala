package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait VerySmallLevel extends SolutionChecker {
    val level =
      """ST
        |oo
        |oo""".stripMargin

    val optsolution = List(Down, Right, Up)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("block is standing") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding)
    }
  }

  test("block is not standing") {
    new Level1 {
      assert(!Block(Pos(1,0), Pos(1,1)).isStanding)
    }
  }

  test("block is legal") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isLegal)
    }
  }

  test("block is not legal") {
    new Level1 {
      assert(!Block(Pos(0,3), Pos(1,3)).isLegal)
    }
  }

  test("start block is at start pos") {
    new Level1 {
      assert(startBlock.isStanding)
      assert(startBlock.isLegal)
      assert(startBlock.b1 == startPos)
      assert(startBlock.b2 == startPos)
    }
  }

  test("neighbours") {
    new Level1 {
      assert(startBlock.neighbors === List(
        (Block(Pos(1,-1), Pos(1,0)), Left),
        (Block(Pos(1,2),  Pos(1,3)), Right),
        (Block(Pos(-1,1), Pos(0,1)), Up),
        (Block(Pos(2,1),  Pos(3,1)), Down)))
    }
  }

  test("legal neighbours") {
    new Level1 {
      assert(startBlock.legalNeighbors === List(
        (Block(Pos(1,2),  Pos(1,3)), Right),
        (Block(Pos(2,1),  Pos(3,1)), Down)))
    }
  }

  test("is done") {
    new Level1 {
      assert(done(Block(Pos(4,7),Pos(4,7))))
    }
  }

  test("is not done because not standing") {
    new Level1 {
      assert(!done(Block(Pos(4,7),Pos(5,7))))
    }
  }

  test("is not done because not on goal") {
    new Level1 {
      assert(!done(Block(Pos(4,8),Pos(4,8))))
    }
  }

  test("neighbors with history is as expected") {
    new Level1 {
      val neighbors = neighborsWithHistory(Block(Pos(1, 1),Pos(1, 1)), List(Left, Up))
      assert(neighbors === Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream)
    }
  }

  test("new neighbors only is as expected") {
    new Level1 {
      val newNeighbors = newNeighborsOnly(
        neighborsWithHistory(Block(Pos(1, 1),Pos(1, 1)), List(Left, Up)),
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )

      assert(newNeighbors === Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream)
    }
  }

  test("small level from") {
    new VerySmallLevel {
      assert(from(Stream((startBlock, List())), Set(startBlock)) === Set(
        (Block(Pos(0,0), Pos(0,0)), List()),
        (Block(Pos(1,0), Pos(2,0)), List(Down)),
        (Block(Pos(1,1), Pos(2,1)), List(Right, Down)),
        (Block(Pos(0,1), Pos(0,1)), List(Up, Right, Down))
      ).toStream)
    }
  }

  test("paths from start") {
    new VerySmallLevel {
      assert(pathsFromStart === Set(
        (Block(Pos(0,0), Pos(0,0)), List()),
        (Block(Pos(1,0), Pos(2,0)), List(Down)),
        (Block(Pos(1,1), Pos(2,1)), List(Right, Down)),
        (Block(Pos(0,1), Pos(0,1)), List(Up, Right, Down))
      ).toStream)
    }
  }

  test("paths to goal") {
    new VerySmallLevel {
      assert(pathsToGoal === Set(
        (Block(Pos(0,1), Pos(0,1)), List(Up, Right, Down))
      ).toStream)
    }
  }

  test("small optimal") {
    new VerySmallLevel {
      assert(solution === optsolution)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solution === optsolution)
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }
}
