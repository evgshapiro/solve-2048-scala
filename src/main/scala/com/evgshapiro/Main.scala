package com.evgshapiro

import com.evgshapiro.model.Board

import scala.util.control.Breaks.{break, breakable}

object Main {

  def main(args: Array[String]): Unit = {
    var step = 0
    val bar = 1.to(32).map(_ => "-").reduce { case (a, b) => a + b }
    var b = Option(Board.newGameBoard())
    b.foreach(x => println(x.prettyString))
    val start = System.nanoTime()
    while (b.nonEmpty) {
      val a = b.flatMap(Board.suggest)
      step += 1
      if (step % 100 == 0) println(s"Average time per move: ${(System.nanoTime()- start)/step}ns")
      println(s"Action $step: $a")
      println(bar)
      b = a.flatMap(av => b.flatMap(_.action(av)))
      b.foreach(x => println(x.prettyString))
    }
    println("Game over")
  }

}
