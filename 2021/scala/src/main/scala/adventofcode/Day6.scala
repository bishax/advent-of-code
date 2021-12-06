package day6

import scala.io.Source
import org.w3c.dom.css.Counter

val test_data = "3,4,3,1,2"
val puzzle_data =
  Source.fromResource("adventofcode/day6.txt").getLines.toList.head

val PERIOD = 7
val NOOBFACTOR = 2

object part1:
  case class Fish(days_left: Int)

  def parse(raw_data: String): Seq[Fish] =
    raw_data.split(",").map(_.toInt).map(Fish(_))
  def update(acc: Seq[Fish], day: Int) =
    acc.flatMap(fish => {
      if fish.days_left == 0 then Seq(Fish(PERIOD + NOOBFACTOR - 1), Fish(PERIOD - 1))
      else Seq(Fish(fish.days_left - 1))
    })
  def apply(raw_data: String, days: Int): Int =
    val init = parse(raw_data)
    (1 to days).foldLeft(init)(update).length

object part2:
  import math.Integral.Implicits.infixIntegralOps

  def parse(raw_data: String): Map[Int, BigInt] =
    raw_data
      .split(",")
      .map(_.toInt)
      .foldLeft(Map.empty[Int, BigInt])((acc, x) => {
        acc.updated(x, (acc.getOrElse(x, 0): BigInt) + 1)
      })

  def update(acc: Map[Int, BigInt], day: Int) =
    val num_zeros: BigInt = acc.getOrElse(0, 0)
    acc
      .removed(0)
      .updated(PERIOD, (acc.getOrElse(PERIOD, 0): BigInt) + num_zeros)
      .updated(PERIOD + NOOBFACTOR, num_zeros)
      .map((k, v) => (k - 1, v))

  def apply(raw_data: String, days: Int): BigInt =
    val init = parse(raw_data)

    (1 to days).foldLeft(init)(update).values.sum

@main def run() =
  println(part1.apply(test_data, 80))
  println(part1.apply(puzzle_data, 80))

  println(part2.apply(test_data, 256))
  println(part2.apply(puzzle_data, 256))
