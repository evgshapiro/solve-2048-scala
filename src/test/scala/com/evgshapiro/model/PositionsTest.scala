package com.evgshapiro.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import Positions._

class PositionsTest extends AnyFlatSpec with should.Matchers {
  "Position" should "pass construction-deconstruction round trip" in {
    val c = 0.until(4)
    val positions = for {y <- c; x <- c} yield (y, x) -> Position(y, x)
    positions.map(_._1) shouldBe positions.map { case (_, p) => p.y -> p.x }
  }

  it should "detect first/last row" in {
    0.to(3).map(x => Position(0, x)).forall(_.isFirstRow) shouldBe true
    0.to(3).map(x => Position(1, x)).forall(_.isFirstRow) shouldBe false
    0.to(3).map(x => Position(3, x)).forall(_.isLastRow) shouldBe true
    0.to(3).map(x => Position(2, x)).forall(_.isLastRow) shouldBe false
  }

  it should "detect first/last col" in {
    0.to(3).map(x => Position(x, 0)).forall(_.isFirstCol) shouldBe true
    0.to(3).map(x => Position(x, 1)).forall(_.isFirstCol) shouldBe false
    0.to(3).map(x => Position(x, 3)).forall(_.isLastCol) shouldBe true
    0.to(3).map(x => Position(x, 2)).forall(_.isLastCol) shouldBe false
  }

  it should "move next row/col" in {
    Position(0, 0).nextRow shouldBe Position(1, 0)
    Position(2, 3).nextRow shouldBe Position(3, 3)
    Position(0, 0).nextCol shouldBe Position(0, 1)
    Position(2, 2).nextCol shouldBe Position(2, 3)
    Position(2, 3).nextCol shouldBe Position(3, 0) // TODO: do we want that?
  }

  "Forward PositionIterator" should "be able to iterate forward" in {
    val ps = PositionIterator.topLeftToBottomRight.asIterator.take(20).toSeq
    ps shouldBe 0.until(16)
  }

  it should "be initialized as available next" in {
    PositionIterator.topLeftToBottomRight.current.asYXTuple shouldBe 0 -> 0
    PositionIterator.topLeftToBottomRight.hasNext shouldBe true
  }

  it should "store skip mask" in {
    val p = PositionIterator.topLeftToBottomRight.withSkipMask(3, 1)
    p.skipMask shouldBe 3
    p.skipValue shouldBe 1
  }

  it should "skip first row with masking" in {
    val ps = PositionIterator.topLeftToBottomRight.skipFirstRow
    ps.asIterator.count(_.isFirstRow) shouldBe 0
  }

  it should "skip last row with masking" in {
    val ps = PositionIterator.topLeftToBottomRight.skipLastRow
    ps.asIterator.count(_.isLastRow) shouldBe 0
  }

  it should "skip first col with masking" in {
    val ps = PositionIterator.topLeftToBottomRight.skipFirstCol
    ps.asIterator.count(_.isFirstCol) shouldBe 0
  }

  it should "skip last col with masking" in {
    val ps = PositionIterator.topLeftToBottomRight.skipLastCol
    ps.asIterator.count(_.isLastCol) shouldBe 0
  }

  it should "throw an exception on iterating past the end" in {
    val i = 0.to(15).foldLeft(PositionIterator.topLeftToBottomRight) { case (it, _) => it.iterate }
    an [Exception] should be thrownBy i.iterate
  }

  "Backward PositionIterator" should "be able to iterate backward" in {
    val ps = PositionIterator.bottomRightToTopLeft.asIterator.take(20).toSeq
    ps shouldBe 15.to(0, -1)

  }
}
