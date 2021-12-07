package day7

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

val test_data = "16,1,2,0,4,2,7,1,2,14"
val puzzle_data =
  Source.fromResource("adventofcode/day7.txt").getLines.toList.head

def parse(data: String): Seq[Int] = data.split(",").map(_.toInt)

object part1:
  def calculate_cost(align_at: Int, pts: Seq[Int]): Int =
    pts.map((x: Int) => (x - align_at).abs).sum

  def apply(raw_data: String): Int =
    val pts = parse(raw_data)
    pts.map(calculate_cost(_, pts)).min

object part2:
  def calculate_cost(align_at: Int, pts: Seq[Int]): Int =
    pts
      .map((x: Int) => (x - align_at).abs)
      .map(n => ((n * (n + 1)) / 2))
      .sum

  def apply(raw_data: String): Int =
    parse(raw_data)
      .pipe(pts =>
        (pts.min to pts.max)
          .map(calculate_cost(_, pts))
          .min
      )

@main def run() =
  println(part1(test_data))
  println(part1(puzzle_data))
  println("====")
  println(part2(test_data))
  println(part2(puzzle_data))
