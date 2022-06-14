package com.evgshapiro.model

import scala.collection.mutable
import Board.*
import com.evgshapiro.model.Positions._

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq
import scala.util.Random

enum Action {
  case Left, Right, Up, Down
}

class Board(board: Array[Int] = new Array[Int](16)) {
  require(board.length == 16)

  private val r = ThreadLocalRandom.current()

  def addRandomTile(): Boolean = {
    val v = if (r.nextDouble() > 0.1) 2 else 4
    val empty = emptyCellsCount
    if (empty == 0) false
    else {
      val n = r.nextInt(empty)
      setEmptyCell(n, v)
      true
    }
  }

  def action(a: Action): Option[Board] = {
    Option(copy()).filter(b =>
      b.fallCombine(a) && b.addRandomTile()
    )
  }

  def fallCombine(a: Action): Boolean = {
    val (it, mergeSkip, nbr, pit) = a match {
      case Action.Left => (topLeftToBottomRight, firstCol, prevCol, PositionIterator.topLeftToBottomRight)
      case Action.Right => (bottomRightToTopLeft, lastCol, nextCol, PositionIterator.bottomRightToTopLeft)
      case Action.Up => (topLeftToBottomRight, firstRow, prevRow, PositionIterator.topLeftToBottomRight)
      case Action.Down => (bottomRightToTopLeft, lastRow, nextRow, PositionIterator.bottomRightToTopLeft)
    }
    val f1 = fall(pit, a)
    val m = merge(it.view.filterNot(mergeSkip), nbr)
    fall(pit, a)
    f1 || m
  }

  private def firstRow(c: YX): Boolean = c._1 == 0
  private def lastRow(c: YX): Boolean = c._1 == L
  private def firstCol(c: YX): Boolean = c._2 == 0
  private def lastCol(c: YX): Boolean = c._2 == L

  private def fall(yx: PositionIterator, a: Action): Boolean = {
    val rightOrDown = a == Action.Down || a == Action.Right
    val horizontal = a == Action.Left || a == Action.Right
    var counters: Int = if (rightOrDown) 0x03030303 else 0
    var it = yx
    var changed = false
    while (it.hasNext) {
      val c = it.current
      val v = valueAt(c)
      if (v != 0) {
        val index = if (horizontal) c.y else c.x
        val counter = (counters >> (index * 8)) & 0xff
        val target = if (horizontal) c.withX(counter) else c.withY(counter)
        val inc = 1 << (index * 8)
        if (rightOrDown) counters -= inc else counters += inc
        if (target != c) {
          set(target, v)
          set(c, 0)
          changed = true
        }
      }
      it = it.iterate
    }
    changed
  }

  private def merge(yxs: Iterable[YX], neighbor: YX => YX): Boolean = {
    yxs.foldLeft(false) { case (acc, c) =>
      val vc = valueAt(c)
      val nc = neighbor(c)
      val vnc = valueAt(nc)
      if (vc == vnc && vc != 0) {
        set(nc, vc * 2)
        set(c, 0)
        true
      }
      else acc
    }
  }

  def prettyString: String = {
    board.grouped(4).map(_.mkString("\t")).mkString("\n")
  }

  def copy(): Board = new Board(Array.copyOf[Int](board, 16))

  inline def valueAt(p: Position): Int = board(p.asInt)
  inline def valueAt(c: YX): Int = board(lc(guard(c)))
  inline def isEmpty(c: YX): Boolean = valueAt(c) == 0

  inline def setEmptyCell(ith: Int, v: Int): Unit = {
    var n = 0
    var i = 0
    while (i < board.length){
      n += (if (board(i) == 0) 1 else 0)
      if (n -1 == ith) {
        board(i) = v
        i += board.length
      }
      i += 1
    }
  }

  inline def emptyCellsCount: Int = {
    var n = 0
    var i = 0
    while (i < board.length){
      n += (if (board(i) == 0) 1 else 0)
      i += 1
    }
    n
  }

  inline def set(p: Position, v: Int): Unit = {
    board(p.asInt) = v
  }

  inline def set(c: YX, v: Int): Unit = {
    val t = lc(guard(c))
    board(t) = v
  }

  private inline def lc(c: YX): Int = c._1 * 4 + c._2
  private inline def guard(c: YX): YX =
    if (c._1 >= 0 && c ._1 <= L && c._2 >= 0 && c._2 <= L) c
    else throw new IllegalArgumentException(s"Invalid coordinates: $c")

  private def prevRow(c: YX): YX = (c._1 - 1) -> c._2
  private def nextRow(c: YX): YX = (c._1 + 1) -> c._2
  private def prevCol(c: YX): YX = c._1 -> (c._2 - 1)
  private def nextCol(c: YX): YX = c._1 -> (c._2 + 1)

}

object Board {
  type YX = (Int, Int)
  val size: Int = 4
  val L: Int = size - 1
  private val cs = 0.until(size)

  val topLeftToBottomRight: Seq[YX] = for {y <- cs; x <- cs} yield (y, x)
  val bottomRightToTopLeft: Seq[YX] = topLeftToBottomRight.reverse

  def fromPrettyString(s: String): Board = {
    val values = s.split(Array('\t', ' ', '\n')).filter(_.nonEmpty).map(Integer.parseInt)
    new Board(values)
  }

  def newGameBoard(): Board = {
    val b = new Board()
    b.addRandomTile()
    b.addRandomTile()
    b
  }

  private val actionVec = Action.values.toVector
  def suggest(b: Board): Option[Action] = {
    val actionBoards = actionVec.flatMap { a =>
      // Take 4 samples due to random tile generation
      1.to(4).flatMap(_ => b.action(a).map(s => a -> s))
    }
    val emptyCells = b.emptyCellsCount
    val maxLevel = if (emptyCells > 4) 2 else if (emptyCells < 2) 7 else 5
    suggestInternal(actionBoards.par, 0, maxLevel)
      .orElse(suggestInternal(actionBoards.par, 0, 2))
      .orElse(suggestInternal(actionBoards.par, 0, 0)) // Recommend any legal move
  }

  @tailrec
  private def suggestInternal(q: ParSeq[(Action, Board)], level: Int, maxLevel: Int): Option[Action] = {
    if (level >= maxLevel) {
      val s = q.foldLeft(new Array[Long](Action.values.length)) { case (scores, (a, b)) =>
        scores(a.ordinal) += b.emptyCellsCount
        scores
      }
      val maxScore = s.max
      if (maxScore == 0) {
        // Recommend any legal move
        q.headOption.map(_._1)
      }
      else {
        val ord =
          if (s(0) == maxScore) 0
          else if (s(1) == maxScore) 1
          else if (s(2) == maxScore) 2
          else 3

        Option(Action.fromOrdinal(ord))
      }
    } else {
      val next = q.par.flatMap { case (a, b) =>
        actionVec.flatMap { ap =>
          1.to(2).flatMap(_ => b.action(ap).map(s => a -> s))
        }
      }

      suggestInternal(next, level + 1, maxLevel)
    }
  }

  def fromPairs(vs: (YX, Int)*): Board = {
    val b = new Board()
    vs.foreach { case (c, v) => b.set(c, v) }
    b
  }

}