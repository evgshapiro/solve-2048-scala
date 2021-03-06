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

  def addRandomTile(): Boolean = {
    val r = ThreadLocalRandom.current()
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
    val (it, nbr, pit) = a match {
      case Action.Left => (PositionIterator.topLeftToBottomRight.skipFirstCol, prevCol, PositionIterator.topLeftToBottomRight)
      case Action.Right => (PositionIterator.bottomRightToTopLeft.skipLastCol, nextCol, PositionIterator.bottomRightToTopLeft)
      case Action.Up => (PositionIterator.topLeftToBottomRight.skipFirstRow, prevRow, PositionIterator.topLeftToBottomRight)
      case Action.Down => (PositionIterator.bottomRightToTopLeft.skipLastRow, nextRow, PositionIterator.bottomRightToTopLeft)
    }
    val f1 = fall(pit, a)
    val m = merge(it, nbr)
    fall(pit, a)
    f1 || m
  }

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

  private def merge(yxs: PositionIterator, neighbor: Position => Position): Boolean = {
    var it = yxs
    var r = false
    while (it.hasNext) {
      val c = it.current
      val vc = valueAt(c)
      val nc = neighbor(c)
      val vnc = valueAt(nc)
      if (vc == vnc && vc != 0) {
        set(nc, vc * 2)
        set(c, 0)
        r = true
      }
      it = it.iterate
    }
    r
  }

  def prettyString: String = {
    board.grouped(4).map(_.mkString("\t")).mkString("\n")
  }

  def copy(): Board = new Board(Array.copyOf[Int](board, 16))

  inline def valueAt(p: Position): Int = board(p.asInt)
  inline def valueAt(c: YX): Int = valueAt(Position.apply.tupled(c))

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

  private def prevRow(c: Position): Position = c.prevRow
  private def nextRow(c: Position): Position = c.nextRow
  private def prevCol(c: Position): Position = c.prevCol
  private def nextCol(c: Position): Position = c.nextCol

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
    // Search depth
    val maxLevel =
      emptyCells match {
        case 0 | 1 => 7
        case 2 | 3 | 4 => 5
        case _ => 2
      }
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
    vs.foreach { case (c, v) => b.set(Position(c._1, c._2), v) }
    b
  }

}