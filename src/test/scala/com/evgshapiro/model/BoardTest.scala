package com.evgshapiro.model

import org.scalatest._
import flatspec._
import matchers._

class BoardTest extends AnyFlatSpec with should.Matchers {

  "Board" should "combine elements on Left" in {
    val board = Board.fromPairs((1, 1) -> 2, (1, 2) -> 2, (2, 2) -> 2, (2, 3) -> 4)
    board.fallCombine(Action.Left)
    board.valueAt(1 -> 0) shouldBe 4
    board.valueAt(2 -> 0) shouldBe 2
    board.valueAt(2 -> 1) shouldBe 4
  }

  it should "combine elements from left to right on Left" in {
    val board = Board.fromPairs((0, 0) -> 2, (0, 1) -> 2, (0, 2) -> 2)
    board.fallCombine(Action.Left)
    board.valueAt(0 -> 0) shouldBe 4
    board.valueAt(0 -> 1) shouldBe 2
  }

  it should "combine elements on Right" in {
    val board = Board.fromPairs((1, 1) -> 2, (1, 2) -> 2, (2, 1) -> 2, (2, 2) -> 4)
    board.fallCombine(Action.Right)
    board.valueAt(1 -> 3) shouldBe 4
    board.valueAt(2 -> 2) shouldBe 2
    board.valueAt(2 -> 3) shouldBe 4
  }

  it should "combine elements from right to left on Right" in {
    val board = Board.fromPairs((0, 0) -> 2, (0, 1) -> 2, (0, 2) -> 2)
    board.fallCombine(Action.Right)
    board.valueAt(0 -> 2) shouldBe 2
    board.valueAt(0 -> 3) shouldBe 4
  }

  it should "combine elements on Down" in {
    val board = Board.fromPairs((1, 1) -> 2, (2, 1) -> 2, (1, 2) -> 2, (2, 2) -> 4)
    println(board.prettyString)
    board.fallCombine(Action.Down)
    println(board.prettyString)
    board.valueAt(3 -> 1) shouldBe 4
    board.valueAt(2 -> 2) shouldBe 2
    board.valueAt(3 -> 2) shouldBe 4
  }

  it should "combine elements from bottom to top on Down" in {
    val board = Board.fromPairs((0, 0) -> 2, (1, 0) -> 2, (2, 0) -> 2)
    board.fallCombine(Action.Down)
    board.valueAt(2 -> 0) shouldBe 2
    board.valueAt(3 -> 0) shouldBe 4
  }

  it should "combine elements on Up" in {
    val board = Board.fromPairs((1, 1) -> 2, (2, 1) -> 2, (1, 2) -> 2, (2, 2) -> 4)
    board.fallCombine(Action.Up)
    board.valueAt(0 -> 1) shouldBe 4
    board.valueAt(0 -> 2) shouldBe 2
    board.valueAt(1 -> 2) shouldBe 4
  }

  it should "combine elements from top to bottom on Up" in {
    val board = Board.fromPairs((0, 0) -> 2, (1, 0) -> 2, (2, 0) -> 2)
    board.fallCombine(Action.Up)
    board.valueAt(0 -> 0) shouldBe 4
    board.valueAt(1 -> 0) shouldBe 2
  }

  it should "disallow moving Left when there is no change on board" in {
    val board = Board.fromPairs((0, 0) -> 2)
    val r = board.fallCombine(Action.Left)
    board.valueAt(0 -> 0) shouldBe 2
    r should be (false)
  }

  it should "indicate that nothing falls: Left" in {
    val board = Board.fromPairs(0.until(4).map(i => (i, 0) -> 2) ++ Seq((0, 1) -> 4) : _*)
    board.fallCombine(Action.Left) shouldBe false
  }

  it should "down not working case 1" in {
    val board = Board.fromPairs((1, 1) -> 2, (0, 3) -> 2)
    println(board.prettyString)
    board.fallCombine(Action.Down)
    println(board.prettyString)
    board.valueAt(3 -> 1) shouldBe 2
    board.valueAt(3 -> 3) shouldBe 2
  }

  it should "fill borad with random tiles" in {
    val board = Board.fromPairs()
    1.to(16).foreach { _ =>
      board.addRandomTile()
    }
    board.emptyCellsCount shouldBe 0
  }

  it should "suggest action on sample position 1" in {
    val b = Board.fromPrettyString(
      s"""  2	128	256	8
         |	512	8	4	0
         |	4	0	0	0
         |	2	0	0	0
         |""".stripMargin
    )
    Board.suggest(b) shouldNot be (None)
  }

//  it should "suggest action on sample position 2" in {
//    val b = Board.fromPrettyString(
//      s"""	4	0	2	4
//         |	8	64	256	32
//         |	32	1024	512	128
//         |	8	64	32	4
//         |""".stripMargin
//    )
//    Board.suggest(b) shouldNot be (None)
//  }

  it should "set empty cells" in {
    val b = new Board()
    b.setEmptyCell(0, 2)
    b.setEmptyCell(2, 4)
    b.valueAt(0 -> 0) shouldBe 2
    b.valueAt(0 -> 3) shouldBe 4
  }
}

/*
	2	128	256	8
	512	8	4	0
	4	0	0	0
	2	0	0	0

Action: None
--------------------------------
Game over




	2	8	512	8
	4	8	128	16
	0	64	32	4
	0	0	4	4

Action: Some(Down)
--------------------------------
	0	0	512	2
	0	0	128	8
	2	16	32	16
	4	64	4	8

Action: None
--------------------------------
Game over

*/
