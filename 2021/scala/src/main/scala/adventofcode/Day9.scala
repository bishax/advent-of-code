package day9

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

val test_data = Seq(
  "2199943210",
  "3987894921",
  "9856789892",
  "8767896789",
  "9899965678"
)
val puzzle_data =
  Source.fromResource("adventofcode/day9.txt").getLines.toList

type FloorMap = Seq[Seq[Int]]
type Coord = (Int, Int)

def parse(raw_data: Seq[String]): FloorMap =
  raw_data.map(line => line.map(_.toString.toInt))

def neighbourFinder(width: Int, height: Int)(x: Int, y: Int): Seq[Coord] =
  Seq((x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)).filterNot((x, y) =>
    (x < 0 || x >= width) || (y < 0 || y >= height)
  )

def findLows(floorMap: FloorMap) =
  val height = floorMap.size
  val width = floorMap.head.size
  val neighboursOf = neighbourFinder(width, height)
  for
    x <- (0 until width)
    y <- (0 until height)
    w = floorMap(y)(x)
    if neighboursOf(x, y).forall((xn, yn) => floorMap(yn)(xn) > w)
  yield (x, y)

object part1:
  def apply(data: Seq[String]): Int =
    val floorMap = parse(data)
    findLows(floorMap).map((x, y) => floorMap(y)(x) + 1).sum

def growBasin(floorMap: FloorMap)(low: Coord): Set[Coord] =
  val height = floorMap.size
  val width = floorMap.head.size
  val neighboursOf = neighbourFinder(width, height)
  def inner(pointQueue: List[Coord], basinMembers: Set[Coord]): Set[Coord] =
    pointQueue match
      case Nil => basinMembers
      case (pt @ (x, y)) :: pts =>
        val newBasinMembers =
          neighboursOf(x, y)
            .filter((xn, yn) =>
              floorMap(yn)(xn) > floorMap(y)(x) && floorMap(yn)(xn) < 9
            )
        inner((pts ++ newBasinMembers), (basinMembers ++ newBasinMembers) + pt)
  inner(List(low), Set())

object part2:
  def apply(data: Seq[String]): Int =
    val floorMap = parse(data)
    findLows(floorMap)
      .map(low => growBasin(floorMap)(low).size)
      .sortBy(-_)
      .take(3)
      .reduce(_ * _)

@main def run =
  println(part1(test_data))
  println(part1(puzzle_data))
  println(part2(test_data))
  println(part2(puzzle_data))
