package com.evgshapiro

import com.evgshapiro.model.Board

import scala.util.control.Breaks.{break, breakable}

object Main {

  def main(args: Array[String]): Unit = {
    var step = 0
    val bar = 1.to(32).map(_ => "-").reduce { case (a, b) => a + b }
    var b = Board.newGameBoard()
    println(b.prettyString)
    breakable {
      while (true) {
        val a = Board.suggest(b)
        step += 1
        println(s"Action $step: $a")
        println(bar)
        val bp = a.flatMap(b.action)
        if (bp.isEmpty) {
          println("Game over")
          break
        }
        b = bp.get
        println(b.prettyString)
      }
    }
  }

}
