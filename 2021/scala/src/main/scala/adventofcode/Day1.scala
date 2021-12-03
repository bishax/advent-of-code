package day1

import scala.io.Source

val test_data = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
val puzzle_data =
  Source.fromResource("adventofcode/day1.txt").getLines.map(_.toInt).toList

def day1(data: List[Int], n: Int): Int =
  data.sliding(n).map(_.sum).sliding(2).filter(x => x(0) < x(1)).length
