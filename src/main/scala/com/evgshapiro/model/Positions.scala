package com.evgshapiro.model

object Positions {
  /**
   * Packs coordinates on 2048 board:
   * 0-1 bits -- x coordinate
   * 2-3 bits -- y coordinate
   */
  opaque type Position = Int

  final val mask: Int = (1 << 2) - 1
  final val posMask: Int = (1 << 4) - 1
  final val L = 3

  object Position {
    def apply(y: Int, x: Int): Position = ((y & mask) << 2) | (x & mask)
  }

  extension (p: Position)
    inline def y: Int = (p >> 2) & mask
    inline def x: Int = p & mask
    inline def isFirstRow: Boolean = y == 0
    inline def isLastRow: Boolean = y == L
    inline def isFirstCol: Boolean = x == 0
    inline def isLastCol: Boolean = x == L
    inline def prevRow: Position = p - (1 << 2)
    inline def nextRow: Position = p + (1 << 2)
    inline def prevCol: Position = p - 1
    inline def nextCol: Position = p + 1
    inline def prevPosition: Position = p - 1
    inline def nextPosition: Position = p + 1
    inline def asYXTuple: (Int, Int) = y -> x

  /**
   * Represents an iterator over positions packed into a single integer.
   * Bits are packed as follows:
   * 0-3: current position
   * 5: 0 - iterator moves forward, 1 - backwards
   *
   */
  opaque type PositionIterator = Int

  object PositionIterator {

    val topLeftToBottomRight: PositionIterator = 0
    val bottomRightToTopLeft: PositionIterator = Position(L, L).flipDirection

  }

  private final val lastPositionForward = Position(L, L)
  private final val lastPositionBackward = Position(0, 0)

  extension (pi: PositionIterator)
    inline def hasNext: Boolean = pi >= 0
    inline def current: Position = pi & posMask
    inline def isForward: Boolean = ((pi >> 4) & 1) == 0
    inline def flipDirection: PositionIterator = pi ^ (1 << 4)
    inline def withPosition(p: Position): PositionIterator = (pi & ~posMask) | (p & posMask)
    inline def iterate: PositionIterator = {
      val c = current
      val f = isForward
      val end = f && c == lastPositionForward || !f && c == lastPositionBackward
      if (end) -pi
      else withPosition(if (f) current.nextPosition else current.prevPosition)
    }
    inline def asScalaIterator: Iterator[Position] = Iterator.unfold(pi) { it =>
      if (it.hasNext) Option(it.current -> it.iterate) else None
    }
}