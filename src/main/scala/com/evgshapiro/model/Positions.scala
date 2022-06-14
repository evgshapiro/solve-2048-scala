package com.evgshapiro.model

object Positions {
  /**
   * Packs coordinates on 2048 board:
   * 0-1 bits -- x coordinate
   * 2-3 bits -- y coordinate
   */
  opaque type Position = Int

  private final val mask: Int = (1 << 2) - 1
  private final val posMask: Int = (1 << 4) - 1
  private final val L = 3

  object Position {
    def apply(y: Int, x: Int): Position = ((y & mask) << 2) | (x & mask)
  }

  extension (p: Position)
    inline def y: Int = (p >> 2) & mask
    inline def x: Int = p & mask
    inline def withY(v: Int): Position = (p & ~(mask << 2)) | ((v & mask) << 2)
    inline def withX(v: Int): Position = (p & ~mask) | (v & mask)
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
    inline def asInt: Int = p & posMask
    inline def asYXTuple: (Int, Int) = y -> x

  /**
   * Represents an iterator over positions packed into a single integer. The iterator
   * can skip values that satisfy a condition: value & skip mask = skip value
   * Bits are packed as follows:
   * 0-3: current position
   * 5: 0 - iterator moves forward, 1 - backwards
   * 6-9: skip mask
   * 10-13: skip value
   * 14: enable-skip mask
   * 31: 0 - iterator has more elements, 1 - iterator has been exhausted
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
    inline def isForward: Boolean = ((pi >> 5) & 1) == 0
    inline def flipDirection: PositionIterator = pi ^ (1 << 5)
    inline def isSkipMaskEnabled: Boolean = ((pi >> 14) & 1) == 1
    inline def withPosition(p: Position): PositionIterator = (pi & ~posMask) | (p & posMask)
    inline def withSkipMask(mask: Int, value: Int): PositionIterator = {
      val withMaskSet = (pi & ~(0xff << 6)) | ((((value << 4) | mask) & 0xff) << 6) | (1 << 14)
      withMaskSet.skipCurrentIfNecessary
    }
    inline def skipFirstRow: PositionIterator = withSkipMask(12 /* 1100 */, 0)
    inline def skipLastRow: PositionIterator = withSkipMask(12 /* 1100 */, 12)
    inline def skipFirstCol: PositionIterator = withSkipMask(3 /* 0011 */, 0)
    inline def skipLastCol: PositionIterator = withSkipMask(3 /* 0011 */, 3)

    inline def skipMask: Int = (pi >> 6) & posMask
     inline def skipValue: Int = (pi >> 10) & posMask
    private inline def isLastPosition(c: Position): Boolean = {
      val f = isForward
      f && c == lastPositionForward || !f && c == lastPositionBackward
    }
    private inline def skipCurrentIfNecessary: PositionIterator = {
      val m = isSkipMaskEnabled
      if (!m) pi
      else {
        val x = current
        if (isLastPosition(x)) -pi
        else {
          val sm = skipMask
          val sv = skipValue
          if ((x & sm) == sv) iterate
          else pi
        }
      }
    }
    inline def iterate: PositionIterator = {
      if (!hasNext) throw new IllegalStateException("Iterator has been exhausted.")
      val f = isForward
      var x = current
      if (isLastPosition(x)) -pi
      else {
        val startedLast = isLastPosition(x)
        val m = isSkipMaskEnabled
        val sm = skipMask
        val sv = skipValue
        while {
          x = if (f) x.nextPosition else x.prevPosition
          (m && ((x & sm) == sv)) && !isLastPosition(x)
        } do ()
        if (startedLast) -withPosition(x)
        else withPosition(x)
      }
    }
    inline def asIterator: Iterator[Position] = Iterator.unfold(pi) { it =>
      if (it.hasNext) Option(it.current -> it.iterate) else None
    }
}